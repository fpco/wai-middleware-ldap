{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Wai.Middleware.Auth.LDAP
  ( LDAPAuth(..)
  , ldapParser
  , waiMiddlewareLDAPVersion
  ) where
import           Control.Exception                    (catch)
import           Control.Monad                        (fail, when)
import           Data.Aeson
import qualified Data.ByteString.Base64               as B64
import qualified Data.ByteString.Builder              as B
import qualified Data.ByteString.Char8                as S8
import           Data.Foldable                        (foldl')
import qualified Data.HashMap.Strict                  as HM
import           Data.List                            (intercalate)
import           Data.Maybe                           (fromMaybe)
import           Data.Proxy                           (Proxy (..))
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Version                         (Version)
import           Foreign.ForeignPtr                   (finalizeForeignPtr)
import           LDAP
import           Network.HTTP.Types                   (status200, status403,
                                                       status405, status500,
                                                       status503)
import           Network.HTTP.Types.Method
import           Network.Wai                          (requestMethod,
                                                       responseBuilder)
import           Network.Wai.Middleware.Auth.Provider
import           Network.Wai.Parse
import qualified Paths_wai_middleware_ldap            as Paths
import           Text.Blaze.Html                      (preEscapedToHtml)
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
  { laUrls           :: [String]
  , laBindDN         :: String
  , laBindDNPassword :: String
  , laBaseUserDN     :: Maybe String
  , laUserSearchKey  :: Maybe String
  , laScope          :: LDAPScope
  , laFilter         :: Maybe String
  , laProviderInfo   :: ProviderInfo
  , laDebug          :: Bool
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

data LDAPLogin = LDAPLogin
  { loginUserId   :: String
  , loginPassword :: String
  }



-- | Aeson parser for `LDAPAuth`.
--
-- @since 0.1.0
ldapParser :: ProviderParser
ldapParser = mkProviderParser (Proxy :: Proxy LDAPAuth)


data LoginResult
  = BindFail -- ^ Initial bind failed
  | LoginFailure String -- ^ Login has failed with a reason.
  | LoginSuccess String -- ^ Indicates successfull login and contains user query
                        -- that was used during the search.
  deriving (Show)


instance FromJSON LDAPAuth where
  parseJSON =
    withObject "LDAP Auth Object" $ \obj -> do
      laProviderInfo <-
        obj .:? "provider_info" .!=
        ProviderInfo
        { providerTitle = "LDAP"
        , providerLogoUrl =
            "https://pyrmin.io/gitlab/uploads/project/avatar/98/icon-ldap-big.png"
        , providerDescr = "Authentication using LDAP"
        }
      laDebug <- obj .:? "debug" .!= False
      laUrls <- obj .: "urls"
      when (null laUrls) $ fail "LDAP urls list is empty."
      laBindDN <- obj .: "bind_dn"
      password <- obj .: "bind_dn_password"
      laBindDNPassword <-
        case B64.decode $ encodeUtf8 password of
          Left err     -> fail err
          Right passwd -> return $ S8.unpack passwd
      laBaseUserDN <- obj .:? "base_user_dn"
      laUserSearchKey <- obj .:? "user_search_key"
      scope <- obj .:? "scope" .!= Nothing
      laScope <-
        case (scope :: Maybe String) of
          Nothing         -> return LdapScopeDefault
          Just "default"  -> return LdapScopeDefault
          Just "base"     -> return LdapScopeBase
          Just "onelevel" -> return LdapScopeOnelevel
          Just "subtree"  -> return LdapScopeSubtree
          Just unknown    -> fail $ "Unknown scope: " ++ unknown
      laFilter <- obj .:? "filter"
      return LDAPAuth {..}



-- | Perform authentication against an LDAP server.
loginLDAP :: LDAPAuth -- ^ LDAP provider
          -> LDAPLogin -- ^ LDAP login info
          -> IO LoginResult
loginLDAP (LDAPAuth {..}) (LDAPLogin {..}) = do
  eLdapObj <- bindFirst laUrls []
  case eLdapObj of
    Left errors
      | laDebug ->
        return $
        LoginFailure $
        "Initial binds: <br>" ++ intercalate "<br>" (map show errors)
    Left _ -> return BindFail
    Right (url, ldapObj) -> do
      entries <-
        ldapSearch
          ldapObj
          laBaseUserDN
          laScope
          (Just $ getQuery laFilter)
          LDAPNoAttrs
          True
      finalizeForeignPtr ldapObj
      case entries of
        [] ->
          return $
          LoginFailure $ "User with query: " ++ userQuery ++ " not found."
        [entry] -> loginUser url entry
        _ ->
          return $
          LoginFailure $
          "Too many users found: " ++ intercalate "<br>" (map show entries)
  where
    userQuery =
      fromMaybe "uid" laUserSearchKey ++ "=" ++ escapeSpecialChars loginUserId
    getQuery Nothing  = userQuery
    getQuery (Just q) = "(&(" ++ userQuery ++ ")" ++ q ++ ")"
    bindFirst (url:rest) acc = do
      when laDebug $ print $ "Trying bind with: " ++ url
      ldapObj <- ldapInitialize url
      handleLDAP
        (\e -> finalizeForeignPtr ldapObj >> bindFirst rest (e : acc))
        (do ldapSimpleBind ldapObj laBindDN laBindDNPassword
            return $ Right (url, ldapObj))
    bindFirst _ acc = return $ Left $ reverse acc
    loginUser url (LDAPEntry userDN _) = do
      userLdapObj <- ldapInitialize url
      handleLDAP
        (\e -> do
            finalizeForeignPtr userLdapObj
            return $ LoginFailure $ "User <" ++ userDN ++ "> bind: " ++ show e)
        (do ldapSimpleBind userLdapObj userDN loginPassword
            finalizeForeignPtr userLdapObj
            return $ LoginSuccess userQuery)


instance AuthProvider LDAPAuth where
  getProviderName _ = "ldap"
  getProviderInfo = laProviderInfo
  handleLogin ldapAuth@(LDAPAuth {laDebug}) req _ renderUrl onSuccess onFailure =
    case requestMethod req of
      m
        | m == methodGet ->
          return $
          responseBuilder status200 [] $ loginTemplate Nothing renderUrl
      m
        | m == methodPost -> do
          (params, _) <- parseRequestBodyEx loginFormBodyOptions lbsBackEnd req
          let paramsHM = HM.filter (not . S8.null) $ HM.fromList params
          let withError status errMsg =
                return $
                responseBuilder status [] $
                loginTemplate (Just errMsg) renderUrl
          let toResponse (LoginSuccess userQuery) =
                onSuccess $ S8.pack userQuery
              toResponse (BindFail) =
                withError status503 "Unable to connect to an LDAP servers."
              toResponse (LoginFailure err)
                | laDebug = withError status403 $ T.pack err
                | otherwise = withError status403 "Incorrect username or password."
          case HM.lookup "username" paramsHM of
            Nothing -> withError status200 "Username field is blank."
            Just userId ->
              case HM.lookup "password" paramsHM of
                Nothing -> withError status200 "Password field is blank."
                Just password ->
                  catch
                    (loginLDAP
                       ldapAuth
                       (LDAPLogin
                        { loginUserId = S8.unpack userId
                        , loginPassword = S8.unpack password
                        }) >>=
                     toResponse)
                    (\(e :: IOError) ->
                       withError status500 $
                       if laDebug
                         then T.pack $ show e
                         else "Issue with LDAP server. Try again later.")
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
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js">
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
          #{preEscapedToHtml errMsg}
      <form name="login_form" method="post" action="">
        <div .form-group>
          <label for="id_username">
            Username *
          <input #id_username .form-control maxlength="254" autocomplete="off" name="username" type="text" autofocus required>
        <div .form-group>
          <label for="id_password">
            Password *
          <input #id_password .form-control autocomplete="off" name="password" type="password" required>
        <div .form-group>
          <button .btn .btn-default .pull-right type="submit" name="login_btn">
            Sign In
|]
