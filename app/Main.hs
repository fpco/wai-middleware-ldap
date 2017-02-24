{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where
import           Data.Monoid                               ((<>))
import           Network.Wai.Auth.Executable
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.Auth.LDAP
import           Options.Applicative.Simple


data BasicOptions = BasicOptions
  { configFile :: FilePath
  } deriving (Show)

basicSettingsParser :: Parser BasicOptions
basicSettingsParser =
  BasicOptions <$>
  strOption
    (long "config-file" <>
     short 'c' <>
     metavar "CONFIG" <>
     help "File with configuration for the LDAP Auth application.")


main :: IO ()
main = do
  (BasicOptions {..}, ()) <-
    simpleOptions
      $(simpleVersion waiMiddlewareLDAPVersion)
      "wai-ldap - LDAP Authentication server"
      "Run a protected file server or reverse proxy."
      basicSettingsParser
      empty
  authConfig <- readAuthConfig configFile
  mkMain authConfig [ldapParser] $ \port app -> do
    putStrLn $ "Listening on port " ++ show port
    run port app
