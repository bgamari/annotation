{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Clock
import Data.Time.Format
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Web.Scotty hiding (options)
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status
import System.Directory
import System.FilePath
import Options.Applicative

options :: Parser (FilePath, FilePath, Int, Maybe FilePath)
options =
    (,,,)
      <$> option str (short 'o' <> long "output" <> metavar "DIR" <> help "Annotation output directory")
      <*> option str (short 's' <> long "static" <> metavar "DIR" <> help "Static HTML directory")
      <*> option auto (short 'p' <> long "port" <> metavar "N" <> value 3333 <> help "Port number")
      <*> optional (option str (short 'c' <> long "credentials" <> metavar "FILE" <> help "Credentials file"))

authSettings :: AuthSettings
authSettings = "TREC CAR annotations"

checkCreds :: FilePath -> CheckCreds
checkCreds credsFile username password = do
    creds <- parseCreds <$> BS.readFile credsFile
    return $ lookup username creds == Just password
  where
    parseCreds = concatMap (toPair . BS.words) . BS.lines
    toPair [a,b] = [(a,b)]
    toPair _ = []

main :: IO ()
main = do
    (destDir, staticDir, port, credFile) <- execParser $ info options mempty
    createDirectoryIfMissing True destDir
    scotty port $ do
        mapM_ (middleware . flip basicAuth authSettings . checkCreds) credFile
        post "/annotation" $ do
            res <- runExceptT $ postAnnotation destDir
            case res of
                Left (s, msg) -> text msg >> status s
                Right ()      -> status ok200
        get "/" $ file (staticDir </> "index.html")
        get (regex "/(.+)/$") $ do
           path <- param "1"
           file (staticDir </> path </> "index.html")
        get (regex "/(.+)$") $ do
            path <- param "1"
            isDir <- liftIO $ doesDirectoryExist path
            liftIO $ putStrLn $ path <> " isDir?" <> show isDir
            if isDir
              then redirect $ T.pack (staticDir </> path <> "/bg")
              else file (staticDir </> path)


postAnnotation :: FilePath -> ExceptT (Status, T.Text) ActionM ()
postAnnotation destDir = do
    user <- lift $ T.unpack `fmap` param "user"
    when (null (user :: String)) $ throwE (status500, "expected user name")
    time <- liftIO getCurrentTime
    let fname = destDir </> user<>"-"<>formatTime defaultTimeLocale "%F-%H%M" time<>".json"
    payload <- lift annotationData
    liftIO $ BSL.writeFile fname payload
    return ()

annotationData :: ActionM BSL.ByteString
annotationData = param "qrel"
