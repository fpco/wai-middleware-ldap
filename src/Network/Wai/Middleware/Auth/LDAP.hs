{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Wai.Middleware.Auth.LDAP
  ( LDAPProvider(..)
  , LDAPAuth(..)
  , ldapParser
  , waiMiddlewareLDAPVersion
  ) where
import           Control.Exception                    (catch)
import           Control.Monad                        (fail, when)
import           Data.Aeson
import qualified Data.ByteString.Builder              as B
import qualified Data.ByteString.Char8                as S8
import           Data.Foldable
import           Data.Foldable                        (foldl')
import qualified Data.HashMap.Strict                  as HM
import           Data.Proxy                           (Proxy (..))
import qualified Data.Text                            as T
import           Data.Version                         (Version)
import           LDAP
import           Network.HTTP.Types                   (status200, status303,
                                                       status404, status405,
                                                       status501)
import           Network.HTTP.Types.Method
import           Network.Wai                          (requestMethod,
                                                       responseBuilder)
import           Network.Wai.Middleware.Auth.Provider
import           Network.Wai.Parse
import qualified Paths_wai_middleware_ldap            as Paths
import           Text.Blaze.Html.Renderer.Utf8        (renderHtmlBuilder)
import           Text.Hamlet                          (Render, hamlet)

-- | Current LDAP Auth version
--
-- @since 0.1.0
waiMiddlewareLDAPVersion :: Version
waiMiddlewareLDAPVersion = Paths.version


-- | LDAP Authentication settings
--
-- @since 0.1.0
data LDAPAuth = LDAPAuth
  { laHost           :: String
  , laPort           :: LDAPInt
  , laBindDN         :: String
  , laBindDNPassword :: String
  , laBaseUserDN     :: Maybe String
  , laScope          :: LDAPScope
  }


-- | Special characters in Distinguished Names need to be escaped with \'\\\'.
escapeSpecialChars :: String -> String
escapeSpecialChars = reverse . foldl' escapeAcc ""
  where
    escapeAcc !acc !c
      | c `elem` specialCharsDN = c : '\\' : acc
      | otherwise = c : acc
    specialCharsDN :: String
    specialCharsDN = "=+;,\\\"<>#"

data LDAPUser
  = LDAPUserID String
  | LDAPEmail String



-- | Aeson parser for `LDAPProvider`.
--
-- @since 0.1.0
ldapParser :: ProviderParser
ldapParser = mkProviderParser (Proxy :: Proxy LDAPProvider)


ldapUserIdentity :: LDAPUser -> UserIdentity
ldapUserIdentity (LDAPUserID  uid) = S8.pack uid
ldapUserIdentity (LDAPEmail email) = S8.pack email


data LDAPProvider = LDAPProvider
  { ldapProviderInfo :: ProviderInfo
  , ldapDebug        :: Bool
  , ldapServers      :: [LDAPAuth]
  }

instance FromJSON LDAPProvider where
  parseJSON =
    withObject "LDAP Provider Object" $ \obj -> do
      ldapProviderInfo <-
        obj .:? "provider_info" .!=
        ProviderInfo
        { providerTitle = "LDAP"
        , providerLogoUrl =
            "https://pyrmin.io/gitlab/uploads/project/avatar/98/icon-ldap-big.png"
        , providerDescr = "Authentication using LDAP"
        }
      ldapServers <- obj .: "servers"
      ldapDebug <- obj .:? "debug" .!= False
      when (null ldapServers) $ fail "LDAP server list is empty"
      return $ LDAPProvider {..}


data LoginError
  = BindFail -- ^ Initial bind failed
  | UserNotFound -- ^ User with such username or email wasn't found
  | IncorrectPassword -- ^ Password mismatch.
  | OtherError String -- ^ Useful for debugging
  deriving (Show)


instance FromJSON LDAPAuth where
  parseJSON =
    withObject "LDAP Auth Object" $ \obj -> do
      laHost <- obj .: "host"
      laPort <-
        (fmap (fromIntegral :: Int -> LDAPInt) <$> (obj .:? "port")) .!=
        ldapPort
      laBindDN <- obj .: "bind_dn"
      laBindDNPassword <- obj .: "bind_dn_password"
      laBaseUserDN <- obj .:? "base_user_dn"
      scope <- obj .:? "scope" .!= Nothing
      laScope <-
        case (scope :: Maybe String) of
          Nothing -> return LdapScopeDefault
          Just "default" -> return LdapScopeDefault
          Just "base" -> return LdapScopeBase
          Just "onelevel" -> return LdapScopeOnelevel
          Just "subtree" -> return LdapScopeSubtree
          Just unknown -> fail $ "Unknown scope: " ++ unknown
      return LDAPAuth {..}


getUserQuery :: LDAPUser -> String
getUserQuery (LDAPUserID uid)  = "uid=" ++ escapeSpecialChars uid
getUserQuery (LDAPEmail email) = "email=" ++ escapeSpecialChars email


-- | Perform authentication against an LDAP server.
loginLDAP :: Bool -- ^ Debug mode. If True will produce more descriptive error
                  -- messages.
          -> LDAPAuth -- ^ LDAP provider
          -> LDAPUser -- ^ User query
          -> String -- ^ User password
          -> IO (Either LoginError UserIdentity)
loginLDAP debug LDAPAuth {..} user userPassword = do
  ldapObj <- ldapInit laHost laPort
  handleLDAP
    (\e ->
       return $
       Left $
       if debug
         then OtherError $ "Initial bind: " ++ show e
         else BindFail)
    (do ldapSimpleBind ldapObj laBindDN laBindDNPassword
        entries <-
          ldapSearch
            ldapObj
            laBaseUserDN
            laScope
            (Just $ getUserQuery user)
            LDAPAllUserAttrs
            True
        loginUser entries)
  where
    loginUser [] = return $ Left UserNotFound
    loginUser [LDAPEntry userDN _] = do
      userLdapObj <- ldapInit laHost laPort
      handleLDAP
        (\e ->
           return $
           Left $
           if debug
             then OtherError $ "User <" ++ userDN ++ "> bind: " ++ show e
             else IncorrectPassword)
        (do ldapSimpleBind userLdapObj userDN userPassword
            return $ Right $ ldapUserIdentity user)
    loginUser es =
      return $
      Left $
      if debug
        then OtherError $ "Too many users found: " ++ show es
        else UserNotFound


instance AuthProvider LDAPProvider where
  getProviderName _ = "ldap"
  getProviderInfo = ldapProviderInfo
  handleLogin LDAPProvider {ldapDebug, ldapServers} req suffix renderUrl onSuccess onFailure =
    case requestMethod req of
      m
        | m == methodGet ->
          return $
          responseBuilder status200 [] $ loginTemplate Nothing renderUrl
      m
        | m == methodPost -> do
          (params, _) <- parseRequestBodyEx loginFormBodyOptions lbsBackEnd req
          let paramsHM = HM.filter (not . S8.null) $ HM.fromList params
          let withError errMsg =
                responseBuilder status200 [] $
                loginTemplate (Just errMsg) renderUrl
          case HM.lookup "username" paramsHM of
            Nothing -> return $ withError "Username field is blank."
            Just username ->
              case HM.lookup "password" paramsHM of
                Nothing -> return $ withError "Password field is blank."
                Just password ->
                  let login [] resp = return resp
                      login (ldapServer:rest) resp = do
                        mres <-
                          catch
                            (loginLDAP
                               ldapDebug
                               ldapServer
                               (LDAPUserID $ S8.unpack username)
                               (S8.unpack password))
                            (\(e :: IOError) ->
                               return $
                               Left
                                 (if ldapDebug
                                    then OtherError $ show e
                                    else BindFail))
                        case mres of
                          Right userId -> onSuccess userId
                          Left BindFail ->
                            login
                              rest
                              (withError "Unable to connect to an LDAP servers.")
                          Left _ | not ldapDebug ->
                            login
                              rest
                              (withError "Incorrect username or password.")
                          Left err ->
                            login rest (withError $ T.pack $ show err)
                  in login ldapServers (withError "No LDAP servers available.")
      _ -> onFailure status405 "Method Not Allowed"



-- | Request body options with disabled file uploading.
loginFormBodyOptions :: ParseRequestBodyOptions
loginFormBodyOptions =
  setMaxRequestFileSize 0 $
  setMaxRequestNumFiles 0 defaultParseRequestBodyOptions


loginTemplate :: Maybe T.Text
              -> Render ProviderUrl
              -> B.Builder
loginTemplate mErrMsg =
  renderHtmlBuilder . [hamlet|
$doctype 5
<html>
  <head>
    <title>LDAP Authentication.
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" integrity="sha384-BVYiiSIFeK1dGmJRAkycuHAHRg32OmUcww7on3RYdg4Va+PmSTsz/K68vbdEjh4u" crossorigin="anonymous">
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css" integrity="sha384-rHyoN1iRsVXV4nD0JutlnGaslCJuC7uwjduW9SVrLvRYooPp2bWYgmgJQIXwl/Sp" crossorigin="anonymous">
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" integrity="sha384-Tc5IQib027qvyjSMfHjOMaLkfuWVxZxUPnCJA7l2mCWNIpG9mGCD8wGNIcPD7Txa" crossorigin="anonymous">
    <style>
      .provider-logo {
        max-height: 64px;
        max-width: 64px;
        padding: 5px;
        margin: auto;
        position: absolute;
        top: 0;
        bottom: 0;
        left: 0;
        right: 0;
      }
      .login-container {
        width: 400px;
        position: absolute;
        top: 100px;
        bottom: 0;
        left: 0;
        right: 0;
        margin: auto;
      }
      .provider.media {
        border: 1px solid #e1e1e8;
        padding: 5px;
        height: 82px;
        margin-top: 5px;
      }
      .provider.media:hover {
        background-color: #f5f5f5;
        border: 1px solid #337ab7;
      }
      .provider .media-left {
        height: 70px;
        width: 0px;
        padding-right: 70px;
        position: relative;
      }
      a:hover {
        text-decoration: none;
      }
  <body>
    <div .login-container>
      <h3>LDAP Authentication:
      $maybe errMsg <- mErrMsg
        <div class="alert alert-block alert-danger">
          #{errMsg}
      <form name="login_form" method="post" action="">
        <div .form-group>
          <label for="id_username">
            Username *
          <input #id_username .form-control maxlength="254" autocomplete="off" name="username" type="text">
        <div .form-group>
          <label for="id_password">
            Password *
          <input #id_password .form-control autocomplete="off" name="password" type="password">
        <div .form-group>
          <button .btn .btn-default .pull-right type="submit" name="login_btn">
            Sign In
|]
