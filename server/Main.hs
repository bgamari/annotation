{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Time.Clock
import Data.Time.Format
import Control.Applicative
import Control.Monad (when, unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.List (sort)
import Data.Monoid
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Web.Scotty hiding (options)
import Network.Wai.Middleware.HttpAuth
import Network.Wai
import Network.HTTP.Types.Status
import Network.URI
import System.Directory
import System.FilePath
import System.Directory
import Options.Applicative hiding (header)
import Debug.Trace as Debug


import qualified Data.Text.Lazy.Encoding
import Data.Aeson
import GHC.Generics

import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Html5 ((!))


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
    (destDir, staticDirUnnorm, port, credFile) <- execParser $ info options mempty
    staticDir <- canonicalizePath staticDirUnnorm
    createDirectoryIfMissing True destDir
    scotty port $ do
        mapM_ (middleware . flip basicAuth authSettings . checkCreds) credFile
        post "/annotation" $ do       -- case 1
            res <- runExceptT $ postAnnotation destDir
            case res of
                Left (s, msg) -> text msg >> status s
                Right ()      -> status ok200

        post "/assessment" $ do       -- case 1
            res <- runExceptT $ postAnnotation2 destDir
            case res of
                Left (s, msg) -> text msg >> status s
                Right ()      -> status ok200

        get "/username" $ do
            res <- runExceptT $ authUsername
            case res of
                Left (s, msg)    -> text msg >> status s
                Right (username) -> text (TL.pack $ BS.unpack username) >> status ok200

        get "/list" $ do
            path <- liftIO $ canonicalizePath $ staticDir </> "data"
            fileList <- liftIO $ getDirectoryContents path
            res <- runExceptT $ createDirJson staticDir fileList

            case res of
                Left (s, msg)    -> text msg >> status s
                Right (res)      -> text (decodeByteString res) >> status ok200

        get "/" $ do -- case 2
--             liftIO $ putStrLn "case 2 GET /"
            pathForward staticDir ""

        get (function $ \req -> let path = rawPathInfo req
                                in if "/" `BS.isSuffixOf` path
                                   then Just []
                                   else Nothing) $ do
           pathUnnorm <- drop 1 . T.unpack . TE.decodeUtf8 . rawPathInfo <$> request
--            liftIO $ putStrLn $ "case 3 pathUnnorm = "<> pathUnnorm
           pathForward staticDir pathUnnorm

        get (function $ const $ Just []) $ do
            pathUnnorm <- drop 1 . T.unpack . TE.decodeUtf8 . rawPathInfo <$> request
--             liftIO $ putStrLn $ "case 4 pathUnnorm="<> pathUnnorm
            path <- liftIO $ canonicalizePath $ staticDir </> pathUnnorm
            unless (isChild staticDir path) $ status badRequest400
            isDir <- liftIO $ doesDirectoryExist path
            liftIO $ putStrLn $ path <> " isDir?" <> show isDir
            if isDir
              then redirect $ TL.pack ("/" <> pathUnnorm <> "/")
              else file path

      where visibleFilePath :: FilePath -> Bool
            visibleFilePath ('.':_) = False
            visibleFilePath path = ext `notElem` [".css", ".js"]
              where ext = takeExtension path

            pathForward staticDir pathUnnorm = do
               path <- liftIO $ canonicalizePath $ staticDir </> pathUnnorm
               existFile <- liftIO $ doesFileExist (path </> "index.html")
               if existFile then
                  file (path </> "index.html")
               else do
                  directoryContents <- liftIO $ getDirectoryContents path
                  html $ H.renderHtml $ createDirListing pathUnnorm
                                      $ sort $ filter visibleFilePath directoryContents
-- | Determine whether one canonical path is a child of another.
isChild :: FilePath -> FilePath -> Bool
isChild = \parent child -> go (splitPath parent) (splitPath child)
  where
    go (x:xs) (y:ys)
      | x==y = go xs ys
    go [] _  = True
    go _  _  = False

decodeByteString :: BSL.ByteString -> TL.Text
decodeByteString = Data.Text.Lazy.Encoding.decodeUtf8


postAnnotation :: FilePath -> ExceptT (Status, TL.Text) ActionM ()
postAnnotation destDir = do
    session' <- lift $ TL.unpack `fmap` param "session"
    let session = escapeURIString isUnreserved session'
    when (null (session :: String)) $ throwE (status500, "expected session name")
    time <- liftIO getCurrentTime
    Just authorization <- lift $ header "Authorization"
    let Just (username, _) = extractBasicAuth $ BS.pack $ TL.unpack authorization

    let fname = destDir </> BS.unpack username <>"-"<>session<>"-"<>formatTime defaultTimeLocale "%F-%H%M" time<>".json"
    payload <- lift annotationData
    liftIO $ BSL.writeFile fname payload
    return ()
  where
    annotationData :: ActionM BSL.ByteString
    annotationData = param "qrel"



postAnnotation2 :: FilePath -> ExceptT (Status, TL.Text) ActionM ()
postAnnotation2 destDir = do
    time <- liftIO getCurrentTime
    Just authorization <- lift $ header "Authorization"
    let Just (username, _) = extractBasicAuth $ BS.pack $ TL.unpack authorization

    let fname = destDir </> BS.unpack username <>"-"<>formatTime defaultTimeLocale "%F-%H%M" time<>".json"

    payload <- lift $ jsonData
    let !x = Debug.traceShow (payload :: Aeson.Value) $ 1


    liftIO $ BSL.writeFile fname $ Aeson.encode payload
    return ()


createDirListing :: FilePath -> [FilePath] -> H.Html
createDirListing currentDir fileList = H.docTypeHtml $ do
    let fileLink filename =
            H.li $ H.a ! HA.href url $ H.toHtml filename
          where url = H.stringValue filename -- do not escapeURIString -- the browser will insist to do it for you!

    H.head $ do
        H.meta ! HA.charset "utf-8"
        H.title $ "Directory "<> H.toHtml currentDir
    H.body $ do
        H.h1 $ "Directory "<> H.toHtml currentDir
        H.ul $
          mapM_ fileLink fileList

createDirJson :: FilePath -> [FilePath] -> ExceptT (Status, TL.Text) ActionM (BSL.ByteString)
createDirJson currentDir fileList = do
    let lst = FileListing { pathname = T.pack currentDir
                      , filenames = fmap T.pack fileList
                      }
    return $ Aeson.encode lst

authUsername :: ExceptT (Status, TL.Text) ActionM (BS.ByteString)
authUsername =  do
    Just authorization <- lift $ header "Authorization"
    let Just (username, _) = extractBasicAuth $ BS.pack $ TL.unpack authorization
    return username


data FileListing = FileListing {
        filenames :: [T.Text]
       , pathname :: T.Text
    }
  deriving (Eq, FromJSON, ToJSON, Generic, Show)