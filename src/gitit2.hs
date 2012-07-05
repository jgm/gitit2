{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings #-}
import Network.Gitit2
import Network.Socket hiding (Debug)
import Network.URI
import Yesod
import Yesod.Static
import Network.Wai.Handler.Warp
import Data.FileStore
import Data.Yaml
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import System.IO
import System.Exit

data Master = Master { getGitit :: Gitit }
mkYesod "Master" [parseRoutes|
/ SubsiteR Gitit getGitit
|]

instance Yesod Master where
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent $ do
      addWidget contents
    mmsg <- getMessage
    hamletToRepHtml [hamlet|
        $doctype 5
        <html>
          <head>
             <title>#{title}
             ^{headTags}
          <body>
             $maybe msg  <- mmsg
               <p.message>#{msg}
             ^{bodyTags}
        |]

instance RenderMessage Master FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage Master GititMessage where
    renderMessage x = renderMessage (getGitit x)

instance HasGitit Master where
  maybeUser = return $ Just $ GititUser "Dummy" "dumb@dumber.org"
  requireUser = return $ GititUser "Dummy" "dumb@dumber.org"
  makePage = makeDefaultPage

-- | Ready collection of common mime types. (Copied from
-- Happstack.Server.HTTP.FileServe.)
mimeTypes :: M.Map String ContentType
mimeTypes = M.fromList
        [("xml","application/xml")
        ,("xsl","application/xml")
        ,("js","text/javascript")
        ,("html","text/html")
        ,("htm","text/html")
        ,("css","text/css")
        ,("gif","image/gif")
        ,("jpg","image/jpeg")
        ,("png","image/png")
        ,("txt","text/plain; charset=UTF-8")
        ,("doc","application/msword")
        ,("exe","application/octet-stream")
        ,("pdf","application/pdf")
        ,("zip","application/zip")
        ,("gz","application/x-gzip")
        ,("ps","application/postscript")
        ,("rtf","application/rtf")
        ,("wav","application/x-wav")
        ,("hs","text/plain")]

data Conf = Conf { port            :: Int
                 , listen_address  :: String
                 , wiki_path       :: FilePath
                 , static_dir      :: FilePath
                 , mime_types_file :: Maybe FilePath
                 }

-- | Read a file associating mime types with extensions, and return a
-- map from extensions to types. Each line of the file consists of a
-- mime type, followed by space, followed by a list of zero or more
-- extensions, separated by spaces. Example: text/plain txt text
readMimeTypesFile :: FilePath -> IO (M.Map String ContentType)
readMimeTypesFile f = catch
  ((foldr go M.empty . map words . lines) `fmap` readFile f)
  handleMimeTypesFileNotFound
     where go []     m = m  -- skip blank lines
           go (x:xs) m = foldr (\ext -> M.insert ext $ B.pack x) m xs
           handleMimeTypesFileNotFound e = do
             hPutStrLn stderr $ "Could not parse mime types file.\n" ++ show e
             return mimeTypes

checkListen :: String -> String
checkListen l | isIPv6address l = l
              | isIPv4address l = l
              | otherwise       = error "Gitit.checkListen: Not a valid interface name"

parseConfig :: Object -> Parser Conf
parseConfig o = do
  port' <- o .:? "port" .!= 3000
  static_dir' <- o .:? "static_dir" .!= "static"
  wiki_path' <- o .:? "wiki_path" .!= "wikidata"
  mime_types_file' <- o .:? "mime_types_file"
  listen_address' <- o .:? "listen_address" .!= "0.0.0.0"
  return Conf{ port = port'
             , listen_address = checkListen listen_address'
             , wiki_path = wiki_path'
             , static_dir = static_dir'
             , mime_types_file = mime_types_file'
             }

main :: IO ()
main = do
  res <- decodeEither `fmap` B.readFile "config/settings.yaml"
  conf <- case res of
             Left e  -> do
               hPutStrLn stderr ("Error reading configuration file.\n" ++ e)
               exitWith $ ExitFailure 3
               return undefined
             Right x -> parseMonad parseConfig x
  let fs = gitFileStore $ wiki_path conf
  st <- staticDevel $ static_dir conf
  mimes <- case mime_types_file conf of
                Nothing -> return mimeTypes
                Just f  -> readMimeTypesFile f
  let settings = defaultSettings{ settingsPort = port conf }

  -- open tthe requested interface
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  device <- inet_addr $ listen_address conf
  bindSocket sock $ SockAddrInet (toEnum (port conf)) device
  listen sock 10

  -- in future, could add option to use runSettingsSocket...
  let runner = runSettingsSocket settings sock
  runner =<< toWaiApp
      (Master (Gitit{ config    = GititConfig{
                                     mime_types = mimes
                                  }
                    , filestore = fs
                    , getStatic = st
                    }))
