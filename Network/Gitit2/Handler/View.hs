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
import           Data.ByteString.Lazy (ByteString, fromStrict)
import           Data.ByteString.Lazy.UTF8 (toString)
import qualified Text.Pandoc.UTF8 as UTF8
import           Data.FileStore as FS
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe, isJust, isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
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
import           Text.Pandoc.Shared (stringify, inDirectory)
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
  let feedRoute = staticR $ StaticRoute ["img","icons","feed.png"] []

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
  let langs = syntaxesByFilename $ takeFileName path'
  return $ not (null langs || takeExtension path' == ".svg")
                         -- allow svg to be served as image

getExportFormats :: GH master [(Text, (Text, WikiPage -> GH master (ContentType,Content)))]
getExportFormats = do
  conf <- getConfig
  let repopath = repository_path conf
  -- let userdata = pandoc_user_data conf
  let selfcontained = toSelfContained repopath
  return $
    [ ("Asciidoc", (".txt", basicExport "asciidoc" typePlain $ pureWriter writeAsciiDoc))
    , ("Beamer", (".tex", basicExport "beamer" "application/x-latex" $ pureWriter writeLaTeX))
    , ("ConTeXt", (".tex", basicExport "context" "application/x-context" $ pureWriter writeConTeXt))
    , ("DocBook", (".xml", basicExport "docbook" "application/docbook+xml" $ pureWriter writeDocbook))
    , ("DZSlides", (".html", basicExport "dzslides" typeHtml $ \opts -> selfcontained opts .
                writeDZSlides opts))
    , ("EPUB", (".epub", basicExport "epub" "application/xhtml+xml" $ \opts ->
                 inDirectory repopath . writeEPUB opts))
    , ("Groff man", (".1", basicExport "man" typePlain $ pureWriter writeMan))
    , ("HTML4", (".html", basicExport "html" typeHtml $ \opts -> selfcontained opts . writeHtml4String opts))
    , ("HTML5", (".html", basicExport "html5" typeHtml $ \opts ->
                   selfcontained opts . writeHtml5String opts))
    , ("LaTeX", (".tex", basicExport "latex" "application/x-latex" $ pureWriter writeLaTeX))
    , ("Markdown", (".txt", basicExport "markdown" typePlain $ pureWriter writeMarkdown))
    , ("Mediawiki", (".wiki", basicExport "mediawiki" typePlain $ pureWriter writeMediaWiki))
    , ("ODT", (".odt", basicExport "opendocument" "application/vnd.oasis.opendocument.text"
             $ \opts -> inDirectory repopath . writeODT opts))
    , ("OpenDocument", (".xml", basicExport "opendocument" "application/vnd.oasis.opendocument.text"
                   $ pureWriter writeOpenDocument))
    , ("Org-mode", (".org", basicExport "org" typePlain $ pureWriter writeOrg)) ] ++
    [ ("PDF", (".pdf", basicExport "latex" "application/pdf" $ \opts d ->
                   inDirectory repopath $ makePDF (fromMaybe "pdflatex" $
                     latex_engine conf) writeLaTeX opts d >>= \res ->
                       case res of
                         Left e    -> error $ "Could not produce PDF: " ++ toString e
                         Right pdf -> return pdf)) | isJust (latex_engine conf) ] ++
    [ ("Plain text", (".txt", basicExport "plain" typePlain $ pureWriter writePlain))
    , ("reStructuredText", (".txt", basicExport "rst" typePlain $ pureWriter writeRST))
    , ("RTF", (".rtf", basicExport "rtf" "application/rtf" writeRTFWithEmbeddedImages))
    , ("Textile", (".txt", basicExport "textile" typePlain $ pureWriter writeTextile))
    , ("S5", (".html", basicExport "s5" typeHtml $ \opts ->
                selfcontained opts . writeS5 opts))
    , ("Slidy", (".html", basicExport "slidy" typeHtml $ \opts ->
                selfcontained opts . writeSlidy opts))
    , ("Texinfo", (".texi", basicExport "texinfo" "application/x-texinfo" $ pureWriter writeTexinfo))
    , ("Word docx", (".docx", basicExport "docx"
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            $ \opts -> inDirectory repopath . writeDocx opts))
    ]

pageToHtml :: HasGitit master => WikiPage -> GH master Html
pageToHtml wikiPage =
  return $ writeHtml def{
               writerWrapText = False
             , writerHighlightStyle = Just pygments
             , writerHTMLMathMethod = MathML
             } $ Pandoc nullMeta (wpContent wikiPage)

toSelfContained :: FilePath -> WriterOptions -> String -> IO String
toSelfContained repopath w cont =
  inDirectory repopath $ makeSelfContained w cont

pureWriter :: (WriterOptions -> Pandoc -> String) -> WriterOptions -> Pandoc -> IO String
pureWriter w opts d = return $ w opts d

toWikiPage :: HasGitit master => Html -> GH master (WidgetT master IO ())
toWikiPage rendered = do
  cfg <- config <$> getYesod
  return $ do
    when (use_mathjax cfg) $ addScriptRemote mathjax_url
    toWidget rendered

basicExport :: ToContent a
            => String -> ContentType -> (WriterOptions -> Pandoc -> IO a)
            -> WikiPage -> GH master (ContentType, Content)
basicExport templ contentType writer wikiPage = do
  conf <- getConfig
  template' <- liftIO $ getDefaultTemplate (pandoc_user_data conf) templ
  template <- case template' of
                     Right t  -> return t
                     Left e   -> throw e
  let metadataToVar :: (Text, Value) -> Maybe (String, String)
      metadataToVar (k, String v) = Just (T.unpack k, T.unpack v)
      metadataToVar (k, Bool v)   = Just (T.unpack k, if v then "yes" else "no")
      metadataToVar (k, Number v) = Just (T.unpack k, show v)
      metadataToVar _             = Nothing
  let vars = mapMaybe metadataToVar $ M.toList $ wpMetadata wikiPage
  dzcore <- if templ == "dzslides"
                then liftIO $ do
                  dztempl <- runIOorExplode $ do
                                 setUserDataDir $ pandoc_user_data conf
                                 UTF8.toString <$> readDataFile
                                  $ "dzslides" </> "template.html"
                  return $ unlines
                      $ dropWhile (not . isPrefixOf "<!-- {{{{ dzslides core")
                      $ lines dztempl
                else return ""
  rendered <- liftIO $
               setInputFiles [repository_path conf]
               writer def{
                         writerTemplate = Just template
                       , writerExtensions = if wpLHS wikiPage
                                               then enableExtension
                                                     Ext_literate_haskell
                                                     pandocExtensions
                                               else pandocExtensions
                       , writerTableOfContents = wpTOC wikiPage
                       , writerHTMLMathMethod = MathML
                       , writerVariables = ("dzslides-core",dzcore):vars }
               $ Pandoc (Meta $ M.singleton "title" $ MetaInlines $ wpTitle wikiPage) $ wpContent wikiPage
  return (contentType, toContent rendered)

mathjax_url :: Text
mathjax_url = "//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

sourceToHtml :: HasGitit master
             => FilePath -> ByteString -> GH master Html
sourceToHtml path contents = do
  syntax <- case syntaxByFilename defaultSyntaxMap path of
                    (s:_) -> return s
                    []    -> mzero -- TODO fix?
  let formatOpts = defaultFormatOpts { numberLines = True
                                     , lineAnchors = True }
  return $ formatHtmlBlock formatOpts
         $ tokenize TokenizerConfig{ traceOutput = False
                                   , syntaxMap = defaultSyntaxMap }
                    syntax contents

getMimeType :: FilePath -> GH master ContentType
getMimeType fp = do
  mimeTypes <- mime_types <$> getConfig
  return $ fromMaybe "application/octet-stream"
         $ M.lookup (drop 1 $ takeExtension fp) mimeTypes
