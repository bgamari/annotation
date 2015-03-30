{-# LANGUAGE OverloadedStrings #-}

import Data.Time.Clock
import Data.Time.Format
import Control.Applicative
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS
import Web.Scotty
import Network.HTTP.Types.Status
import System.Directory
import System.FilePath

-- Configuration
destDir = "/home/dietz/annotations"
staticDir = "/home/dietz/unpolished"

main = do
    createDirectoryIfMissing True destDir
    scotty 3333 $ do
        post "/annotation" $ do
            res <- runEitherT postAnnotation
            case res of
                Left (s, msg) -> text msg >> status s
                Right ()      -> status ok200
        get "/" $ file (staticDir </> "/index.html")
        get (regex "/(.+)$") $ do
            path <- T.unpack <$> param "1"
            liftIO $ putStrLn path
            file (staticDir </> path)

postAnnotation :: EitherT (Status, T.Text) ActionM ()
postAnnotation = do
    passwd <- lift $ T.unpack `fmap` param "password"
    when (passwd /= "queripidia") $ left (status403, "incorrect password")
    user <- lift $ T.unpack `fmap` param "user"
    when (null (user :: String)) $ left (status500, "expected user name")
    time <- liftIO getCurrentTime
    let fname = destDir </> user<>"-"<>formatTime defaultTimeLocale "%F-%H%M" time<>".json"
    payload <- lift annotationData
    liftIO $ BS.writeFile fname payload
    return ()

annotationData :: ActionM BS.ByteString
annotationData = param "qrel"
