{-# LANGUAGE CPP #-}

module Network.Gitit2.Handler.View (
  getViewR,
  getExportR,
  getRevisionR,
  makeDefaultPage,
  postPreviewR
  ) where

import           Control.Exception (throw)
import           Control.Monad (when, foldM)
import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.FileStore as FS
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe, isJust, isNothing)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           Data.Yaml
import           Network.Gitit2.Cache
import           Network.Gitit2.Import
import           Network.Gitit2.Page (discussPageFor, isDiscussPage, Page(Page), pageToText, pathForFile, textToPage)
import           Network.Gitit2.WikiPage (WikiPage(..), contentToWikiPage')
import           System.FilePath
import           Text.Blaze.Html hiding (contents)
import           Skylighting
import           Text.Julius (juliusFile)
import           Text.Pandoc
import           Text.Pandoc.PDF (makePDF)
import           Text.Pandoc.SelfContained (makeSelfContained)
import           Text.Pandoc.Shared (stringify)
import           Yesod.AtomFeed
import           Yesod.Static

getViewR :: HasGitit master => Page -> GH master Html
getViewR page = do
  pathForPage page >>= tryCache
  tryCache $ pathForFile page
  view Nothing page

getExportR :: HasGitit master
            => Text
            -> Page
            -> GH master (ContentType, Content)
getExportR format page = do
  exportFormats <- getExportFormats
  case lookup format exportFormats of
         Nothing -> fail "Unrecognized format"
         Just (extension, f) -> do
           path <- pathForPage page
           -- set filename here so it works for cached page
           setFilename $ pageToText page <> extension
           tryCache $ path </> T.unpack format
           mbcont <- getRawContents path Nothing
           case mbcont of
                Nothing   -> fail "Could not get page contents"
                Just cont -> contentsToWikiPage page cont >>=
                               caching (path </> T.unpack format) . f >>=
                               sendResponse

getRevisionR :: HasGitit master => RevisionId -> Page -> GH master Html
getRevisionR rev = view (Just rev)

postPreviewR :: HasGitit master => Page -> GH master Html
postPreviewR page = do
  contents <- lift $ runInputPost $ ireq textField "contents"
  wikipage <- contentsToWikiPage page (fromStrict $ encodeUtf8 contents)
  pageToHtml wikipage

setFilename :: Text -> HandlerT sub (HandlerT master IO) ()
setFilename fname = addHeader "Content-Disposition"
                  $ "attachment; filename=\"" <> fname <> "\""

makeDefaultPage :: HasGitit master => PageLayout -> WidgetT master IO () -> GH master Html
makeDefaultPage layout content = do
  toMaster <- getRouteToParent
  let logoRoute = staticR $ StaticRoute ["img","logo.png"] []
  -- let feedRoute = staticR $ StaticRoute ["img","icons","feed.png"] []

  let searchRoute = toMaster SearchR
  let goRoute = toMaster GoR
  let tabClass :: Tab -> Text
      tabClass t = if t == pgSelectedTab layout then "selected" else ""
  let showTab t = t `elem` pgTabs layout
  exportFormats <- getExportFormats
  lift $ defaultLayout $ do
    addStylesheet $ staticR $ StaticRoute ["css","custom.css"] []
    addScript $ staticR $ StaticRoute ["js","jquery-1.7.2.min.js"] []
    addScript $ staticR $ StaticRoute ["js","bootstrap.min.js"] []
    atomLink (toMaster AtomSiteR) "Atom feed for the wiki"
    toWidget $ [lucius|input.hidden { display: none; } |]
    $(whamletFile "templates/default_page.hamlet")

view :: HasGitit master => Maybe RevisionId -> Page -> GH master Html
view mbrev page = do
  path <- pathForPage page
  mbcont <- getRawContents path mbrev
  case mbcont of
       Just contents -> do
         wikipage <- contentsToWikiPage page contents
         htmlContents <- pageToHtml wikipage
         let mbcache = if wpCacheable wikipage && isNothing mbrev
                          then caching path
                          else id
         mbcache $ layout [ViewTab,EditTab,HistoryTab,DiscussTab]
                            (wpCategories wikipage) htmlContents
       Nothing -> do
         let path' = pathForFile page
         mbcont' <- getRawContents path' mbrev
         is_source <- isSourceFile path'
         case mbcont' of
              Nothing -> do
                 setMessageI (MsgNewPage page)
                 redirect $ EditR page
              Just contents
               | is_source -> do
                   htmlContents <- sourceToHtml path' contents
                   caching path' $ layout [ViewTab,HistoryTab] [] htmlContents
               | otherwise -> do
                  ct <- getMimeType path'
                  let content = toContent contents
                  caching path' (return (ct, content)) >>= sendResponse
   where layout tabs categories cont = do
           toMaster <- getRouteToParent
           contw <- toWikiPage cont
           makePage pageLayout{ pgName = Just page
                              , pgPageTools = True
                              , pgTabs = tabs
                              , pgSelectedTab = if isDiscussPage page
                                                   then DiscussTab
                                                   else ViewTab } $
                    do setTitle $ toMarkup page
                       toWidget $(juliusFile "templates/view.julius")
                       atomLink (toMaster $ AtomPageR page)
                          "Atom link for this page"
                       $(whamletFile "templates/view.hamlet")

contentsToWikiPage :: HasGitit master => Page  -> ByteString -> GH master WikiPage
contentsToWikiPage page contents = do
  conf <- getConfig
  plugins' <- getPlugins
  converter <- wikiLinksConverter (pageToPrefix page)
  let title = pageToText page
  let defaultFormat = default_format conf
  foldM applyPlugin (contentToWikiPage' title contents converter defaultFormat) plugins'
  where
    -- | Convert links with no URL to wikilinks.
    wikiLinksConverter :: Text -> GH master ([Inline] -> String)
    wikiLinksConverter prefix = do
      toMaster <- getRouteToParent
      toUrl <- lift getUrlRender
      return $ T.unpack . toUrl . toMaster . ViewR . textToPage . (T.append prefix) . T.pack . stringify

    pageToPrefix (Page []) = T.empty
    pageToPrefix (Page ps) = T.intercalate "/" $ init ps ++ [T.empty]

isSourceFile :: FilePath -> GH master Bool
isSourceFile path' = do
  let langs = syntaxesByFilename defaultSyntaxMap (takeFileName path')
  return $ not (null langs || takeExtension path' == ".svg")
                         -- allow svg to be served as image

getExportFormats :: GH master [(Text, (Text, WikiPage -> GH master (ContentType,Content)))]
getExportFormats = do
  conf <- getConfig
  let repopath = repository_path conf
  let selfcontained f = \opt pdc -> do
        setResourcePath [repopath]
        setUserDataDir (pandoc_user_data conf)
        res <- f opt pdc
        T.pack <$> makeSelfContained (T.unpack res)
  return $
    [ ("Asciidoc", (".txt", basicExport "asciidoc" typePlain $ pureWriter writeAsciiDoc))
    , ("Beamer", (".tex", basicExport "beamer" "application/x-latex" $ pureWriter writeLaTeX))
    , ("ConTeXt", (".tex", basicExport "context" "application/x-context" $ pureWriter writeConTeXt))
    , ("DocBook v4", (".xml", basicExport "docbook" "application/docbook+xml" $ pureWriter writeDocbook4))
    , ("DocBook v5", (".xml", basicExport "docbook" "application/docbook+xml" $ pureWriter writeDocbook5))
    , ("DZSlides", (".html", basicExport "dzslides" typeHtml $ selfcontained writeDZSlides))
    , ("EPUB v2", (".epub", basicExport "epub" "application/xhtml+xml" $ \opts d -> setResourcePath [repopath] >> writeEPUB2 opts d))
    , ("EPUB v3", (".epub", basicExport "epub" "application/xhtml+xml" $ \opts d -> setResourcePath [repopath] >> writeEPUB3 opts d))
    , ("Groff man", (".1", basicExport "man" typePlain $ pureWriter writeMan))
    , ("HTML4", (".html", basicExport "html" typeHtml $ selfcontained $ pureWriter writeHtml4String))
    , ("HTML5", (".html", basicExport "html5" typeHtml $ selfcontained $ pureWriter writeHtml5String))
    , ("LaTeX", (".tex", basicExport "latex" "application/x-latex" $ pureWriter writeLaTeX))
    , ("Markdown", (".txt", basicExport "markdown" typePlain $ pureWriter writeMarkdown))
    , ("Mediawiki", (".wiki", basicExport "mediawiki" typePlain $ pureWriter writeMediaWiki))
    , ("ODT", (".odt", basicExport "opendocument" "application/vnd.oasis.opendocument.text"
             $ \opts d -> setResourcePath [repopath] >> writeODT opts d))
    , ("OpenDocument", (".xml", basicExport "opendocument" "application/vnd.oasis.opendocument.text"
                   $ pureWriter writeOpenDocument))
    , ("Org-mode", (".org", basicExport "org" typePlain $ pureWriter writeOrg)) ] ++
    [ ("PDF", (".pdf", basicExport "latex" "application/pdf" $ \opts d ->
                         do setResourcePath [repopath]
                            res <- makePDF (fromMaybe "pdflatex" $
                                    latex_engine conf) []
                                    writeLaTeX opts d
                            case res of
                                 Left e -> fail (toString e)
                                 Right r -> return r
                        )) | isJust (latex_engine conf) ] ++
    [ ("Plain text", (".txt", basicExport "plain" typePlain $ pureWriter writePlain))
    , ("reStructuredText", (".txt", basicExport "rst" typePlain $ pureWriter writeRST))
    , ("RTF", (".rtf", basicExport "rtf" "application/rtf" writeRTF))
    , ("Textile", (".txt", basicExport "textile" typePlain $ pureWriter writeTextile))
    , ("S5", (".html", basicExport "s5" typeHtml $ selfcontained $ pureWriter  writeS5))
    , ("Slidy", (".html", basicExport "slidy" typeHtml $ selfcontained $ pureWriter writeSlidy))
    , ("Texinfo", (".texi", basicExport "texinfo" "application/x-texinfo" $ pureWriter writeTexinfo))
    , ("Word docx", (".docx", basicExport "docx"
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            $ \opts d -> setResourcePath [repopath] >> writeDocx opts d))
    ]

pageToHtml :: HasGitit master => WikiPage -> GH master Html
pageToHtml wikiPage = do
  let res = runPure $ writeHtml5 def{
               writerWrapText = WrapNone
             , writerHighlightStyle = Just pygments
             , writerHTMLMathMethod = MathML
             } $ Pandoc nullMeta (wpContent wikiPage)
  case res of
       Left err -> throw err
       Right r  -> return r

pureWriter :: (WriterOptions -> Pandoc -> PandocPure Text)
           -> WriterOptions -> Pandoc -> PandocIO Text
pureWriter w opts d =
  case runPure (w opts d) of
       Left e  -> throw e
       Right x -> return x

toWikiPage :: HasGitit master => Html -> GH master (WidgetT master IO ())
toWikiPage rendered = do
  cfg <- config <$> getYesod
  return $ do
    when (use_mathjax cfg) $ addScriptRemote mathjax_url
    toWidget rendered

basicExport :: ToContent a
            => String -> ContentType
            -> (WriterOptions -> Pandoc -> PandocIO a)
            -> WikiPage -> GH master (ContentType, Content)
basicExport templ contentType writer wikiPage = do
  conf <- getConfig
  let metadataToVar :: (Text, Value) -> Maybe (String, String)
      metadataToVar (k, String v) = Just (T.unpack k, T.unpack v)
      metadataToVar (k, Bool v)   = Just (T.unpack k, if v then "yes" else "no")
      metadataToVar (k, Number v) = Just (T.unpack k, show v)
      metadataToVar _             = Nothing
  let vars = mapMaybe metadataToVar $ M.toList $ wpMetadata wikiPage
  dzcore <- if templ == "dzslides"
                then liftIO $ do
                  res <- runIO (do setUserDataDir $ pandoc_user_data conf
                                   getDefaultTemplate "dzslides")
                  dztempl <- case res of
                                  Left e  -> throw e
                                  Right r -> return r
                  return $ unlines
                      $ dropWhile (not . isPrefixOf "<!-- {{{{ dzslides core")
                      $ lines dztempl
                else return ""
  res <- liftIO $ runIO
               (do -- TODO better error handling
                   setInputFiles [repository_path conf]
                   template <- getDefaultTemplate templ
                   writer def{
                             writerTemplate = Just template
                           , writerExtensions =
                               if wpLHS wikiPage
                                  then enableExtension Ext_literate_haskell
                                                       pandocExtensions
                                  else pandocExtensions
                           , writerTableOfContents = wpTOC wikiPage
                           , writerHTMLMathMethod = MathML
                           , writerVariables =
                               ("dzslides-core",dzcore):vars }
                     $ Pandoc (Meta $ M.singleton "title"
                     $ MetaInlines $ wpTitle wikiPage)
                     $ wpContent wikiPage)
  case res of
         Right rendered -> return (contentType, toContent rendered)
         Left e -> throw e

mathjax_url :: Text
mathjax_url = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

sourceToHtml :: HasGitit master
             => FilePath -> ByteString -> GH master Html
sourceToHtml path contents = do
  syntax <- case syntaxesByFilename defaultSyntaxMap path of
                    (s:_) -> return s
                    []    -> undefined -- TODO fix?
  let formatOpts = defaultFormatOpts { numberLines = True
                                     , lineAnchors = True }
  case tokenize TokenizerConfig{ traceOutput = False
                               , syntaxMap = defaultSyntaxMap }
                    syntax (decodeUtf8 $ toStrict contents) of
        Right toks -> return $ formatHtmlBlock formatOpts toks
        Left err   -> error err

getMimeType :: FilePath -> GH master ContentType
getMimeType fp = do
  mimeTypes <- mime_types <$> getConfig
  return $ fromMaybe "application/octet-stream"
         $ M.lookup (drop 1 $ takeExtension fp) mimeTypes
