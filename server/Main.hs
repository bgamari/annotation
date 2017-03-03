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
import qualified Data.ByteString.Lazy as BS
import Web.Scotty hiding (options)
import Network.HTTP.Types.Status
import System.Directory
import System.FilePath
import Options.Applicative

options :: Parser (FilePath, FilePath, Int)
options =
    (,,)
      <$> option str (short 'o' <> long "output" <> metavar "DIR" <> help "Annotation output directory")
      <*> option str (short 's' <> long "static" <> metavar "DIR" <> help "Static HTML directory")
      <*> option auto (short 'p' <> long "port" <> metavar "N" <> value 3333 <> help "Port number")

main = do
    (destDir, staticDir, port) <- execParser $ info options mempty
    createDirectoryIfMissing True destDir
    scotty port $ do
        post "/annotation" $ do
            res <- runExceptT $ postAnnotation destDir
            case res of
                Left (s, msg) -> text msg >> status s
                Right ()      -> status ok200
        get "/" $ file (staticDir </> "/index.html")
        get (regex "/(.+)$") $ do
            path <- T.unpack <$> param "1"
            liftIO $ putStrLn path
            file (staticDir </> path)

postAnnotation :: FilePath -> ExceptT (Status, T.Text) ActionM ()
postAnnotation destDir = do
    passwd <- lift $ T.unpack `fmap` param "password"
    when (passwd /= "queripidia") $ throwE (status403, "incorrect password")
    user <- lift $ T.unpack `fmap` param "user"
    when (null (user :: String)) $ throwE (status500, "expected user name")
    time <- liftIO getCurrentTime
    let fname = destDir </> user<>"-"<>formatTime defaultTimeLocale "%F-%H%M" time<>".json"
    payload <- lift annotationData
    liftIO $ BS.writeFile fname payload
    return ()

annotationData :: ActionM BS.ByteString
annotationData = param "qrel"
