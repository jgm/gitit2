{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, TypeFamilies,
    OverloadedStrings #-}
import Network.Gitit2
import Yesod
import Yesod.Static
import Data.FileStore
import Data.Yaml
import qualified Data.ByteString as B
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

parseConfig :: FromJSON a => Object -> Parser a
parseConfig o = do
  o .: "port"

main :: IO ()
main = do
  res <- decodeEither `fmap` B.readFile "config/settings.yaml"
  port <- case res of
             Left e  -> do
               hPutStrLn stderr ("Error reading configuration file.\n" ++ e)
               exitWith $ ExitFailure 3
               return 0
             Right x -> parseMonad parseConfig x
  let conf = GititConfig{ wiki_path = "wikidata"
                        , mime_types = mimeTypes }
  let fs = gitFileStore $ wiki_path conf
  st <- staticDevel "static"
  warpDebug port $ Master (Gitit{ config    = conf
                                , filestore = fs
                                , getStatic = st
                                })
