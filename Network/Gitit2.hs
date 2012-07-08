{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleInstances,
             ScopedTypeVariables, TupleSections #-}
module Network.Gitit2 ( GititConfig (..)
                      , HtmlMathMethod (..)
                      , Page (..)
                      , PageFormat (..)
                      , readPageFormat
                      , HasGitit (..)
                      , Gitit (..)
                      , GititUser (..)
                      , GititMessage (..)
                      , Route (..)
                      , Tab (..)
                      , PageLayout (..)
                      , pageLayout
                      , makeDefaultPage
                      ) where

import Prelude hiding (catch)
import Control.Exception (catch)
import qualified Data.Map as M
import Yesod hiding (MsgDelete)
import Yesod.Static
import Language.Haskell.TH hiding (dyn)
import Data.Ord (comparing)
import Data.List (inits, find, sortBy, isPrefixOf)
import Data.FileStore as FS
import Data.Char (toLower)
import System.FilePath
import Text.Pandoc
import Text.Pandoc.Shared (stringify, inDirectory, readDataFile)
import Text.Pandoc.SelfContained (makeSelfContained)
import Text.Pandoc.Builder (toList, text)
import Control.Applicative
import Control.Monad (when, filterM, mplus)
import qualified Data.Text as T
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 (toString)
import Text.Blaze.Html hiding (contents)
import Blaze.ByteString.Builder (toByteStringIO)
import Text.HTML.SanitizeXSS (sanitizeAttribute)
import Data.Monoid (Monoid, mappend, mconcat)
import Data.Maybe (mapMaybe)
import System.Random (randomRIO)
import Control.Exception (throw, handle, try)
import Text.Highlighting.Kate
import Data.Time (getCurrentTime, addUTCTime)
import Yesod.AtomFeed
import Yesod.Default.Handlers (getFaviconR, getRobotsR)
import Data.Yaml
import System.Directory

-- This is defined in GHC 7.04+, but for compatibility we define it here.
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | A Gitit wiki.  For an example of how a Gitit subsite
-- can be integrated into another Yesod app, see @src/gitit.hs@
-- in the package source.
data Gitit = Gitit{ config        :: GititConfig  -- ^ Wiki config options.
                  , filestore     :: FileStore    -- ^ Filestore with pages.
                  , getStatic     :: Static       -- ^ Static subsite.
                  }

instance Yesod Gitit

-- | Configuration for a gitit wiki.
data GititConfig = GititConfig{
       mime_types       :: M.Map String ContentType -- ^ Table of mime types
     , default_format   :: PageFormat               -- ^ Default format for wiki pages
     , wiki_path        :: FilePath                 -- ^ Path to wiki
     , use_mathjax      :: Bool                     -- ^ Link to mathjax script
     , feed_days        :: Int                      -- ^ Days back for feed entries
     , pandoc_user_data :: Maybe FilePath           -- ^ Pandoc userdata directory
     , use_cache        :: Bool                     -- ^ Cache pages and files
     , cache_dir        :: FilePath                 -- ^ Path to cache
     }

data HtmlMathMethod = UseMathML | UseMathJax | UsePlainMath

-- | Path to a wiki page.  Page and page components can't begin with '_'.
data Page = Page [Text] deriving (Show, Read, Eq)

-- for now, we disallow @*@ and @?@ in page names, because git filestore
-- does not deal with them properly, and darcs filestore disallows them.
instance PathMultiPiece Page where
  toPathMultiPiece (Page x) = x
  fromPathMultiPiece []     = Nothing
  fromPathMultiPiece xs@(_:_) =
     if any (\x ->  "_" `T.isPrefixOf` x ||
                    "*" `T.isInfixOf` x ||
                    "?" `T.isInfixOf` x ||
                    ".." `T.isInfixOf` x ||
                    "/_" `T.isInfixOf` x) xs
                    then Nothing
                    else Just (Page xs)

pageToText :: Page -> Text
pageToText (Page xs) = T.intercalate "/" xs

textToPage :: Text -> Page
textToPage x = Page $ T.splitOn "/" x

instance ToMarkup Page where
  toMarkup = toMarkup . pageToText

instance ToMessage Page where
  toMessage = pageToText

instance ToMarkup (Maybe Page) where
  toMarkup (Just x) = toMarkup x
  toMarkup Nothing  = ""

-- | A user.
data GititUser = GititUser{ gititUserName  :: String
                          , gititUserEmail :: String
                          } deriving Show

-- | A tab in the page layout.
data Tab  = ViewTab
          | EditTab
          | HistoryTab
          | DiscussTab
          | DiffTab
          deriving (Eq, Show)

-- | Page layout.
data PageLayout = PageLayout{
    pgName           :: Maybe Page
  , pgRevision       :: Maybe String
  , pgPrintable      :: Bool
  , pgPageTools      :: Bool
  , pgSiteNav        :: Bool
  , pgTabs           :: [Tab]
  , pgSelectedTab    :: Tab
  }

-- | Default page layout.
pageLayout :: PageLayout
pageLayout = PageLayout{
    pgName           = Nothing
  , pgRevision       = Nothing
  , pgPrintable      = False
  , pgPageTools      = False
  , pgSiteNav        = True
  , pgTabs           = []
  , pgSelectedTab    = ViewTab
  }

-- | The Boolean is True for literate Haskell.
data PageFormat = Markdown Bool | RST Bool | LaTeX Bool | HTML Bool | Textile Bool
                  deriving (Read, Show, Eq)

readPageFormat :: Text -> Maybe PageFormat
readPageFormat s =
  case T.toLower s' of
       "markdown"  -> Just $ Markdown lhs
       "textile"   -> Just $ Textile lhs
       "latex"     -> Just $ LaTeX lhs
       "html"      -> Just $ HTML lhs
       "rst"       -> Just $ RST lhs
       _           -> Nothing
 where (s',rest) = T.break (=='+') s
       lhs = rest == "+lhs"

data WikiPage = WikiPage {
    wpName        :: Text
  , wpFormat      :: PageFormat
  , wpTOC         :: Bool
  , wpLHS         :: Bool
  , wpTitle       :: [Inline]
  , wpCategories  :: [Text]
  , wpMetadata    :: M.Map Text Value
  , wpCacheable   :: Bool
  , wpContent     :: [Block]
} deriving (Show)

-- Create GititMessages.
mkMessage "Gitit" "messages" "en"

-- | The master site containing a Gitit subsite must be an instance
-- of this typeclass.
-- TODO: replace the user functions with isAuthorized from Yesod typeclass?
class (Yesod master, RenderMessage master FormMessage,
       RenderMessage master GititMessage) => HasGitit master where
  -- | Return user information, if user is logged in, or nothing.
  maybeUser   :: GHandler sub master (Maybe GititUser)
  -- | Return user information or redirect to login page.
  requireUser :: GHandler sub master GititUser
  -- | Gitit subsite page layout.
  makePage :: PageLayout -> GWidget Gitit master () -> GHandler Gitit master RepHtml

-- Create routes.
mkYesodSub "Gitit" [ ClassP ''HasGitit [VarT $ mkName "master"]
 ] [parseRoutesNoCheck|
/ HomeR GET
/_help HelpR GET
/_static StaticR Static getStatic
/_index IndexBaseR GET
/_index/*Page  IndexR GET
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/_random RandomR GET
/_raw/*Page RawR GET
/_edit/*Page  EditR GET
/_revision/#RevisionId/*Page RevisionR GET
/_revert/#RevisionId/*Page RevertR GET
/_update/#RevisionId/*Page UpdateR POST
/_create/*Page CreateR POST
/_delete/*Page DeleteR GET POST
/_search SearchR POST
/_go GoR POST
/_upload UploadR GET POST
/_diff/#RevisionId/#RevisionId/*Page DiffR GET
/_history/#Int/*Page HistoryR GET
/_activity/#Int ActivityR GET
/_atom AtomSiteR GET
/_atom/*Page AtomPageR GET
/_export/*Page ExportR POST
/_expire/*Page ExpireR POST
/*Page     ViewR GET
|]

makeDefaultPage :: HasGitit master => PageLayout -> GWidget Gitit master () -> GHandler Gitit master RepHtml
makeDefaultPage layout content = do
  toMaster <- getRouteToMaster
  let logoRoute = toMaster $ StaticR $ StaticRoute ["img","logo.png"] []
  let feedRoute = toMaster $ StaticR $ StaticRoute ["img","icons","feed.png"] []
  let searchRoute = toMaster SearchR
  let goRoute = toMaster GoR
  let tabClass :: Tab -> Text
      tabClass t = if t == pgSelectedTab layout then "selected" else ""
  let showTab t = t `elem` pgTabs layout
  printLayout <- lookupGetParam "print"
  defaultLayout $ do
    addStylesheet $ toMaster $ StaticR $
      case printLayout of
           Just _  -> StaticRoute ["css","print.css"] []
           Nothing -> StaticRoute ["css","custom.css"] []
    addScript $ toMaster $ StaticR $ StaticRoute ["js","jquery-1.7.2.min.js"] []
    atomLink (toMaster AtomSiteR) "Atom feed for the wiki"
    toWidget $ [lucius|input.hidden { display: none; } |]
    [whamlet|
    <div #doc3 .yui-t1>
      <div #yui-main>
        <div #maincol .yui-b>
          <div #userbox>
          $maybe page <- pgName layout
            <ul .tabs>
              $if showTab ViewTab
                $if isDiscussPage page
                  <li class=#{tabClass ViewTab}>
                    <a href=@{toMaster $ ViewR $ discussedPage page}>_{MsgPage}</a>
                $else
                  <li class=#{tabClass ViewTab}>
                    <a href=@{toMaster $ ViewR page}>_{MsgView}</a>
              $if showTab EditTab
                <li class=#{tabClass EditTab}>
                  <a href=@{toMaster $ EditR page}>_{MsgEdit}</a>
              $if showTab HistoryTab
                <li class=#{tabClass HistoryTab}>
                  <a href=@{toMaster $ HistoryR 1 page}>_{MsgHistory}</a>
              $if showTab DiscussTab
                <li class=#{tabClass DiscussTab}><a href=@{toMaster $ ViewR $ discussPageFor page}>_{MsgDiscuss}</a>
          <div #content>
            ^{content}
      <div #sidebar .yui-b .first>
        <div #logo>
          <a href=@{toMaster HomeR}><img src=@{logoRoute} alt=logo></a>
        $if pgSiteNav layout
          <div .sitenav>
            <fieldset>
              <legend>Site
              <ul>
                <li><a href=@{toMaster HomeR}>_{MsgFrontPage}</a>
                <li><a href=@{toMaster $ IndexBaseR}>_{MsgDirectory}</a>
                <li><a href="">_{MsgCategories}</a>
                <li><a href=@{toMaster $ RandomR}>_{MsgRandomPage}</a>
                <li><a href=@{toMaster $ ActivityR 1}>_{MsgRecentActivity}</a>
                <li><a href=@{toMaster UploadR}>_{MsgUploadFile}</a></li>
                <li><a href=@{toMaster AtomSiteR} type="application/atom+xml" rel="alternate" title="ATOM Feed">_{MsgAtomFeed}</a> <img alt="feed icon" src=@{feedRoute}>
                <li><a href=@{toMaster HelpR}>_{MsgHelp}</a></li>
              <form method="post" action=@{searchRoute} id="searchform">
               <input type="text" name="patterns" id="patterns">
               <input type="submit" name="search" id="search" value="_{MsgSearch}">
              <form method="post" action=@{goRoute} id="goform">
                <input type="text" name="gotopage" id="gotopage">
                <input type="submit" name="go" id="go" value="_{MsgGo}">
        $if pgPageTools layout
          <div .pagetools>
            $maybe page <- pgName layout
              <fieldset>
                <legend>This page</legend>
                <ul>
                  <li><a href=@{toMaster $ RawR page}>_{MsgRawPageSource}</a>
                  <li><a href="@{toMaster $ ViewR page}?print">_{MsgPrintableVersion}</a>
                  <li><a href=@{toMaster $ DeleteR page}>_{MsgDeleteThisPage}</a>
                  <li><a href=@{toMaster $ AtomPageR page} type="application/atom+xml" rel="alternate" title="This page's ATOM Feed">_{MsgAtomFeed}</a> <img alt="feed icon" src=@{feedRoute}>
                <form method="post" #exportbox action=@{toMaster $ ExportR page}>
                  <select name="format">
                    $forall (f,_) <- exportFormats
                      <option value=#{f}>#{f}
                  <input type="submit" id="export" name="export" value=_{MsgExport}>
  |]

-- HANDLERS and utility functions, not exported:

-- | Convert links with no URL to wikilinks.
convertWikiLinks :: Inline -> GHandler Gitit master Inline
convertWikiLinks (Link ref ("", "")) = do
  toMaster <- getRouteToMaster
  toUrl <- getUrlRender
  let route = ViewR $ textToPage $ T.pack $ stringify ref
  return $ Link ref (T.unpack $ toUrl $ toMaster route, "")
convertWikiLinks x = return x

addWikiLinks :: Pandoc -> GHandler Gitit master Pandoc
addWikiLinks = bottomUpM convertWikiLinks

sanitizePandoc :: Pandoc -> Pandoc
sanitizePandoc = bottomUp sanitizeBlock . bottomUp sanitizeInline
  where sanitizeBlock (RawBlock _ _) = Text.Pandoc.Null
        sanitizeBlock (CodeBlock (id',classes,attrs) x) =
          CodeBlock (id', classes, sanitizeAttrs attrs) x
        sanitizeBlock x = x
        sanitizeInline (RawInline _ _) = Str ""
        sanitizeInline (Code (id',classes,attrs) x) =
          Code (id', classes, sanitizeAttrs attrs) x
        sanitizeInline (Link lab (src,tit)) = Link lab (sanitizeURI src,tit)
        sanitizeInline (Image alt (src,tit)) = Link alt (sanitizeURI src,tit)
        sanitizeInline x = x
        sanitizeURI src = case sanitizeAttribute ("href", T.pack src) of
                               Just (_,z) -> T.unpack z
                               Nothing    -> ""
        sanitizeAttrs = mapMaybe sanitizeAttr
        sanitizeAttr (x,y) = case sanitizeAttribute (T.pack x, T.pack y) of
                                  Just (w,z) -> Just (T.unpack w, T.unpack z)
                                  Nothing    -> Nothing

pathForPage :: Page -> GHandler Gitit master FilePath
pathForPage p = return $ T.unpack (toMessage p) <.> "page"

pathForFile :: Page -> GHandler Gitit master FilePath
pathForFile p = return $ T.unpack $ toMessage p

pageForPath :: FilePath -> GHandler Gitit master Page
pageForPath fp = return $ textToPage $ T.pack $
  if takeExtension fp == ".page"
     then dropExtension fp
     else fp

isDiscussPage :: Page -> Bool
isDiscussPage (Page xs) = case reverse xs of
                               (x:_) -> "@" `T.isPrefixOf` x
                               _     -> False

discussPageFor :: Page -> Page
discussPageFor (Page xs)
  | isDiscussPage (Page xs) = Page xs
  | otherwise               = Page $ init xs ++ ["@" <> last xs]

discussedPage :: Page -> Page
discussedPage (Page xs)
  | isDiscussPage (Page xs) = Page $ init xs ++ [T.drop 1 $ last xs]
  | otherwise               = Page xs

isPageFile :: FilePath -> GHandler Gitit master Bool
isPageFile f = return $ takeExtension f == ".page"

allPageFiles :: GHandler Gitit master [FilePath]
allPageFiles = do
  fs <- filestore <$> getYesodSub
  liftIO (index fs) >>= filterM isPageFile

isDiscussPageFile :: FilePath -> GHandler Gitit master Bool
isDiscussPageFile ('@':xs) = isPageFile xs
isDiscussPageFile _ = return False

isSourceFile :: FilePath -> GHandler Gitit master Bool
isSourceFile path' = do
  let langs = languagesByFilename $ takeFileName path'
  return $ not (null langs || takeExtension path' == ".svg")
                         -- allow svg to be served as image

-- TODO : make the front page configurable
getHomeR :: HasGitit master => GHandler Gitit master RepHtml
getHomeR = getViewR (Page ["Front Page"])

-- TODO : make the help page configurable
getHelpR :: HasGitit master => GHandler Gitit master RepHtml
getHelpR = getViewR (Page ["Help"])

getRandomR :: HasGitit master => GHandler Gitit master RepHtml
getRandomR = do
  fs <- filestore <$> getYesodSub
  files <- liftIO $ index fs
  pages <- mapM pageForPath =<< filterM (fmap not . isDiscussPageFile)
                            =<<filterM isPageFile files
  pagenum <- liftIO $ randomRIO (0, length pages - 1)
  let thepage = pages !! pagenum
  toMaster <- getRouteToMaster
  redirect $ toMaster $ ViewR thepage

getRawR :: HasGitit master => Page -> GHandler Gitit master RepPlain
getRawR page = do
  path <- pathForPage page
  mbcont <- getRawContents path Nothing
  case mbcont of
       Nothing       -> do
         path' <- pathForFile page
         mbcont' <- getRawContents path' Nothing
         case mbcont' of
              Nothing   -> notFound
              Just cont -> return $ RepPlain $ toContent cont
       Just cont -> return $ RepPlain $ toContent cont

getDeleteR :: HasGitit master => Page -> GHandler Gitit master RepHtml
getDeleteR page = do
  requireUser
  fs <- filestore <$> getYesodSub
  path <- pathForPage page
  pageTest <- liftIO $ try $ latest fs path
  fileToDelete <- case pageTest of
                       Right _        -> return path
                       Left  FS.NotFound -> do
                         path' <- pathForFile page
                         fileTest <- liftIO $ try $ latest fs path'
                         case fileTest of
                              Right _     -> return path' -- a file
                              Left FS.NotFound  -> fail (show FS.NotFound)
                              Left e      -> fail (show e)
                       Left e        -> fail (show e)
  toMaster <- getRouteToMaster
  makePage pageLayout{ pgName = Just page
                     , pgTabs = []
                     } $ do
    [whamlet|
      <h1>#{page}</h1>
      <div #deleteform>
        <form method=post action=@{toMaster $ DeleteR page}>
          <p>_{MsgConfirmDelete page}
          <input type=text class=hidden name=fileToDelete value=#{fileToDelete}>
          <input type=submit value=_{MsgDelete}>
    |]

postDeleteR :: HasGitit master => Page -> GHandler Gitit master RepHtml
postDeleteR page = do
  user <- requireUser
  fs <- filestore <$> getYesodSub
  toMaster <- getRouteToMaster
  mr <- getMessageRender
  fileToDelete <- runInputPost $ ireq textField "fileToDelete"
  liftIO $ FS.delete fs (T.unpack fileToDelete)
            (Author (gititUserName user) (gititUserEmail user))
            (T.unpack $ mr $ MsgDeleted page)
  setMessageI $ MsgDeleted page
  redirect (toMaster HomeR)

getViewR :: HasGitit master => Page -> GHandler Gitit master RepHtml
getViewR page = do
  conf <- config <$> getYesodSub
  if use_cache conf
     then do
          path <- pathForPage page
          path' <- pathForFile page
          mbcache <- lookupCache path
          mbcache' <- lookupCache path'
          case mbcache `mplus` mbcache' of
               Just f  -> do
                 isPage <- isPageFile f
                 ct <- if isPage
                          then return typeHtml
                          else getMimeType f
                 sendFile ct f
               Nothing -> view Nothing page
     else view Nothing page

getMimeType :: FilePath -> GHandler Gitit master ContentType
getMimeType fp = do
  mimeTypes <- mime_types . config <$> getYesodSub
  return $ maybe "application/octet-stream" id
         $ M.lookup (drop 1 $ takeExtension fp) mimeTypes

getRevisionR :: HasGitit master => RevisionId -> Page -> GHandler Gitit master RepHtml
getRevisionR rev = view (Just rev)

view :: HasGitit master => Maybe RevisionId -> Page -> GHandler Gitit master RepHtml
view mbrev page = do
  toMaster <- getRouteToMaster
  path <- pathForPage page
  mbcont <- getRawContents path mbrev
  case mbcont of
       Just contents -> do
         htmlContents <- contentsToWikiPage page contents >>= pageToHtml
         caching path $ layout [ViewTab,EditTab,HistoryTab,DiscussTab] htmlContents
       Nothing -> do
         path' <- pathForFile page
         mbcont' <- getRawContents path' mbrev
         is_source <- isSourceFile path'
         case mbcont' of
              Nothing -> do
                 setMessageI (MsgNewPage page)
                 redirect (toMaster $ EditR page)
              Just contents
               | is_source -> do
                   htmlContents <- sourceToHtml path' contents
                   caching path' $ layout [ViewTab,HistoryTab] htmlContents
               | otherwise -> do
                  ct <- getMimeType path'
                  caching path' $ sendResponse (ct, toContent contents)
   where layout tabs cont = do
           toMaster <- getRouteToMaster
           makePage pageLayout{ pgName = Just page
                              , pgPageTools = True
                              , pgTabs = tabs
                              , pgSelectedTab = if isDiscussPage page
                                                   then DiscussTab
                                                   else ViewTab } $
                    do setTitle $ toMarkup page
                       toWidget [julius|
                                   $(document).keypress(function(event) {
                                       if (event.which == 18) {
                                          $.post("@{toMaster $ ExpireR page}");
                                          window.location.reload(true);
                                          };
                                       });
                                |]
                       atomLink (toMaster $ AtomPageR page)
                          "Atom link for this page"
                       [whamlet|
                         <h1 .title>#{page}
                         $maybe rev <- mbrev
                           <h2 .revision>#{rev}
                         ^{toWikiPage cont}
                       |]

getIndexBaseR :: HasGitit master => GHandler Gitit master RepHtml
getIndexBaseR = getIndexFor []

getIndexR :: HasGitit master => Page -> GHandler Gitit master RepHtml
getIndexR (Page xs) = getIndexFor xs

getIndexFor :: HasGitit master => [Text] -> GHandler Gitit master RepHtml
getIndexFor paths = do
  fs <- filestore <$> getYesodSub
  listing <- liftIO $ directory fs $ T.unpack $ T.intercalate "/" paths
  let isDiscussionPage (FSFile f) = isDiscussPageFile f
      isDiscussionPage (FSDirectory _) = return False
  prunedListing <- filterM (fmap not . isDiscussionPage) listing
  let updirs = inits $ filter (not . T.null) paths
  toMaster <- getRouteToMaster
  let process (FSFile f) = do
        Page page <- pageForPath f
        ispage <- isPageFile f
        let route = toMaster $ ViewR $ Page (paths ++ page)
        return (if ispage then ("page" :: Text) else "upload", route, toMessage $ Page page)
      process (FSDirectory f) = do
        Page page <- pageForPath f
        let route = toMaster $ IndexR $ Page (paths ++ page)
        return ("folder", route, toMessage $ Page page)
  entries <- mapM process prunedListing
  makePage pageLayout{ pgName = Nothing } $ [whamlet|
    <h1 .title>
      $forall up <- updirs
        ^{upDir toMaster up}
    <div .index>
      <ul>
        $forall (cls,route,name) <- entries
          <li .#{cls}>
            <a href=@{route}>#{name}</a>
  |]

upDir :: (Route Gitit -> Route master) -> [Text] -> GWidget Gitit master ()
upDir toMaster fs = do
  let (route, lastdir) = case reverse fs of
                          (f:_)  -> (IndexR $ Page fs, f)
                          []     -> (IndexBaseR, "\x2302")
  [whamlet|<a href=@{toMaster $ route}>#{lastdir}/</a>|]

getRawContents :: HasGitit master
               => FilePath
               -> Maybe RevisionId
               -> GHandler Gitit master (Maybe ByteString)
getRawContents path rev = do
  fs <- filestore <$> getYesodSub
  liftIO $ handle (\e -> if e == FS.NotFound then return Nothing else throw e)
         $ Just <$> retrieve fs path rev

pageToHtml :: HasGitit master => WikiPage -> GHandler Gitit master Html
pageToHtml wikiPage = do
  return $ writeHtml defaultWriterOptions{
               writerWrapText = False
             , writerHtml5 = True
             , writerHighlight = True
             , writerHTMLMathMethod = MathML Nothing
             } $ Pandoc (Meta [] [] []) (wpContent wikiPage)

stripHeader :: [ByteString] -> (ByteString,ByteString)
stripHeader (x:xs)
  | isHeaderStart x = let (hs, bs) = break isHeaderEnd xs
                      in  case bs of
                             []     -> (B.unlines (x:xs), B.empty)
                             (_:ys) -> (B.unlines hs, B.unlines ys)
  | otherwise = (B.empty, B.unlines (x:xs))
 where isHeaderStart z = ["---"] == B.words z
       isHeaderEnd   z = ["..."] == B.words z
stripHeader [] = (B.empty, B.empty)

contentsToWikiPage :: HasGitit master => Page  -> ByteString -> GHandler Gitit master WikiPage
contentsToWikiPage page contents = do
  conf <- config <$> getYesodSub
  let (h,b) = stripHeader $ B.lines contents
  let h' = mconcat $ B.toChunks h
  let metadata :: (M.Map Text Value) = maybe M.empty id $ decode h'
  let def = defaultParserState{ stateSmart = True }
  let formatStr = case M.lookup "format" metadata of
                         Just (String s) -> s
                         _               -> ""
  let format = maybe (default_format conf) id $ readPageFormat formatStr
  let (reader, lhs) = case format of
                        Markdown l -> (readMarkdown def{stateLiterateHaskell = l},l)
                        Textile  l -> (readTextile def{stateLiterateHaskell = l},l)
                        LaTeX    l -> (readLaTeX def{stateLiterateHaskell = l},l)
                        RST      l -> (readRST def{stateLiterateHaskell = l},l)
                        HTML     l -> (readHtml def{stateLiterateHaskell = l},l)
  let fromBool (Bool t) = t
      fromBool _        = False
  let toc = maybe False fromBool (M.lookup "toc" metadata)
  let categories = case M.lookup "categories" metadata of
                        Just (String t) -> T.words $ T.replace "," " " t
                        _               -> []
  let doc = reader $ toString b
  Pandoc _ blocks <- sanitizePandoc <$> addWikiLinks doc
  return $ WikiPage {
             wpName        = pageToText page
           , wpFormat      = format
           , wpTOC         = toc
           , wpLHS         = lhs
           , wpTitle       = toList $ text $ T.unpack $ pageToText page
           , wpCategories  = categories
           , wpMetadata    = metadata
           , wpCacheable   = True
           , wpContent     = blocks
         }

sourceToHtml :: HasGitit master
             => FilePath -> ByteString -> GHandler Gitit master Html
sourceToHtml path contents = do
  let formatOpts = defaultFormatOpts { numberLines = True, lineAnchors = True }
  return $ formatHtmlBlock formatOpts $
     case languagesByExtension $ takeExtension path of
        []    -> highlightAs "" $ toString contents
        (l:_) -> highlightAs l $ toString contents

mathjax_url :: Text
mathjax_url = "https://d3eoax9i5htok0.cloudfront.net/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

toWikiPage :: HasGitit master => Html -> GWidget Gitit master ()
toWikiPage rendered = do
  cfg <- config <$> lift getYesodSub
  when (use_mathjax cfg) $ addScriptRemote mathjax_url
  toWidget rendered

postSearchR :: HasGitit master => GHandler Gitit master RepHtml
postSearchR = do
  patterns <- runInputPost $ ireq textField "patterns"
  searchResults $ T.words patterns

postGoR :: HasGitit master => GHandler Gitit master RepHtml
postGoR = do
  gotopage <- runInputPost $ ireq textField "gotopage"
  let gotopage' = T.toLower gotopage
  allPages <- allPageFiles
  let allPageNames = map (T.pack . dropExtension) allPages
  let findPage f   = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = gotopage' == T.toLower f
  let prefixMatch f = gotopage' `T.isPrefixOf` T.toLower f
  toMaster <- getRouteToMaster
  case (findPage exactMatch `mplus` findPage insensitiveMatch `mplus`
        findPage prefixMatch) of
       Just m  -> redirect $ toMaster $ ViewR $ textToPage m
       Nothing -> searchResults $ T.words gotopage

searchResults :: HasGitit master => [Text] -> GHandler Gitit master RepHtml
searchResults patterns = do
  fs <- filestore <$> getYesodSub
  matchLines <- if null patterns
                   then return []
                   else liftIO $ search fs SearchQuery{
                                             queryPatterns =
                                                map T.unpack patterns
                                           , queryWholeWords = True
                                           , queryMatchAll = True
                                           , queryIgnoreCase = True
                                           }
  let contentMatches = map matchResourceName matchLines
  allPages <- allPageFiles
  let slashToSpace = map (\c -> if c == '/' then ' ' else c)
  let inPageName pageName' x = x `elem`
          (words $ slashToSpace $ dropExtension pageName')
  let matchesPatterns pageName' = not (null patterns) &&
       all (inPageName (map toLower pageName'))
           (map (T.unpack . T.toLower) patterns)
  let pageNameMatches = filter matchesPatterns allPages
  let allMatchedFiles = [f | f <- allPages, f `elem` contentMatches
                                     || f `elem` pageNameMatches ]
  let matchesInFile f =  mapMaybe (\x -> if matchResourceName x == f
                                            then Just (matchLine x)
                                            else Nothing) matchLines
  let matches = map (\f -> (f, matchesInFile f)) allMatchedFiles
  let relevance (f, ms) = length ms + if f `elem` pageNameMatches
                                         then 100
                                         else 0
  let matches' = reverse $ sortBy (comparing relevance) matches
  let matches'' = map (\(f,c) -> (textToPage $ T.pack $ dropExtension f, c)) matches'
  toMaster <- getRouteToMaster
  makePage pageLayout{ pgName = Nothing
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $ do
    toWidget [julius|
      function toggleMatches(obj) {
        var pattern = $('#pattern').text();
        var matches = obj.next('.matches')
        matches.slideToggle(300);
        if (obj.html() == '\u25BC') {
            obj.html('\u25B2');
          } else {
            obj.html('\u25BC');
          };
        }
      $(document).ready(function (){
         $('a.showmatch').attr('onClick', 'toggleMatches($(this));');
         $('pre.matches').hide();
         $('a.showmatch').show();
         });
    |]
    [whamlet|
      <h1>#{T.unwords patterns}
      <ol>
        $forall (page, cont) <- matches''
          <li>
            <a href=@{toMaster $ ViewR page}>#{page}
            $if not (null cont)
               <a href="#" .showmatch>&#x25BC;
            <pre .matches>#{unlines cont}
    |]

getEditR :: HasGitit master => Page -> GHandler Gitit master RepHtml
getEditR page = do
  requireUser
  fs <- filestore <$> getYesodSub
  path <- pathForPage page
  mbcont <- getRawContents path Nothing
  let contents = case mbcont of
                       Nothing -> ""
                       Just c  -> toString c
  mbrev <- case mbcont of
                 Nothing -> return Nothing
                 Just _  -> pathForPage page >>= \f ->
                              liftIO (Just <$> latest fs f)
  edit False contents mbrev page

getRevertR :: HasGitit master
           => RevisionId -> Page -> GHandler Gitit master RepHtml
getRevertR rev page = do
  requireUser
  path <- pathForPage page
  mbcont <- getRawContents path (Just rev)
  case mbcont of
       Nothing       -> notFound
       Just contents -> edit True (toString contents) (Just rev) page

edit :: HasGitit master
     => Bool               -- revert?
     -> String             -- contents to put in text box
     -> Maybe RevisionId   -- unless new page, Just id of old version
     -> Page
     -> GHandler Gitit master RepHtml
edit revert txt mbrevid page = do
  requireUser
  let contents = Textarea $ T.pack $ txt
  mr <- getMessageRender
  let comment = if revert
                   then mr $ MsgReverted $ maybe "" id mbrevid
                   else ""
  (form, enctype) <- generateFormPost $ editForm
                     $ Just Edit{ editContents = contents
                                , editComment = comment }
  toMaster <- getRouteToMaster
  let route = toMaster $ case mbrevid of
                    Just revid -> UpdateR revid page
                    Nothing    -> CreateR page
  showEditForm page route enctype $ do
    when revert $ toWidget [julius|
       $(document).ready(function (){
          $('textarea').attr('readonly','readonly').attr('style','color: gray;');
          }); |]
    form

showEditForm :: HasGitit master
             => Page
             -> Route master
             -> Enctype
             -> GWidget Gitit master ()
             -> GHandler Gitit master RepHtml
showEditForm page route enctype form = do
  makePage pageLayout{ pgName = Just page
                     , pgTabs = [ViewTab,EditTab,HistoryTab,DiscussTab]
                     , pgSelectedTab = EditTab } $ do
    [whamlet|
      <h1>#{page}</h1>
      <div #editform>
        <form method=post action=@{route} enctype=#{enctype}>
          ^{form}
          <input type=submit>
    |]

postUpdateR :: HasGitit master
          => RevisionId -> Page -> GHandler Gitit master RepHtml
postUpdateR revid page = update' (Just revid) page

postCreateR :: HasGitit master
            => Page -> GHandler Gitit master RepHtml
postCreateR page = update' Nothing page

update' :: HasGitit master
       => Maybe RevisionId -> Page -> GHandler Gitit master RepHtml
update' mbrevid page = do
  user <- requireUser
  ((result, widget), enctype) <- runFormPost $ editForm Nothing
  fs <- filestore <$> getYesodSub
  toMaster <- getRouteToMaster
  let route = toMaster $ case mbrevid of
                  Just revid  -> UpdateR revid page
                  Nothing     -> CreateR page
  case result of
       FormSuccess r -> do
         let auth = Author (gititUserName user) (gititUserEmail user)
         let comm = T.unpack $ editComment r
         let cont = filter (/='\r') $ T.unpack $ unTextarea $ editContents r
         path <- pathForPage page
         case mbrevid of
           Just revid -> do
              mres <- liftIO $ modify fs path revid auth comm cont
              case mres of
                   Right () -> do
                      expireCache path
                      redirect $ toMaster $ ViewR page
                   Left mergeinfo -> do
                      setMessageI $ MsgMerged revid
                      edit False (mergeText mergeinfo)
                           (Just $ revId $ mergeRevision mergeinfo) page
           Nothing -> do
             expireCache path
             liftIO $ save fs path auth comm cont
             redirect $ toMaster $ ViewR page
       _ -> showEditForm page route enctype widget

data Edit = Edit { editContents :: Textarea
                 , editComment  :: Text
                 } deriving Show

editForm :: HasGitit master
         => Maybe Edit
         -> Html
         -> MForm Gitit master (FormResult Edit, GWidget Gitit master ())
editForm mbedit = renderDivs $ Edit
    <$> areq textareaField (fieldSettingsLabel MsgPageSource)
           (editContents <$> mbedit)
    <*> areq commentField (fieldSettingsLabel MsgChangeDescription)
           (editComment <$> mbedit)
  where commentField = check validateNonempty textField
        validateNonempty y
          | T.null y = Left MsgValueRequired
          | otherwise = Right y


getDiffR :: HasGitit master
         => RevisionId -> RevisionId -> Page -> GHandler Gitit master RepHtml
getDiffR fromRev toRev page = do
  fs <- filestore <$> getYesodSub
  pagePath <- pathForPage page
  filePath <- pathForFile page
  rawDiff <- liftIO
             $ catch (diff fs pagePath (Just fromRev) (Just toRev))
             $ \e -> case e of
                      FS.NotFound -> diff fs filePath (Just fromRev) (Just toRev)
                      _           -> throw e
  let classFor B = ("unchanged" :: Text)
      classFor F = "deleted"
      classFor S = "added"
  makePage pageLayout{ pgName = Just page
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $
   [whamlet|
     <h1 .title>#{page}
     <h2 .revision>#{fromRev} &rarr; #{toRev}
     <pre>
        $forall (t,xs) <- rawDiff
           <span .#{classFor t}>#{unlines xs}
     |]

getHistoryR :: HasGitit master
            => Int -> Page -> GHandler Gitit master RepHtml
getHistoryR start page = do
  let items = 20 -- items per page
  fs <- filestore <$> getYesodSub
  pagePath <- pathForPage page
  filePath <- pathForFile page
  path <- liftIO
          $ catch (latest fs pagePath >> return pagePath)
          $ \e -> case e of
                   FS.NotFound -> latest fs filePath >> return filePath
                   _           -> throw e
  let offset = start - 1
  hist <- liftIO $ drop offset <$>
           history fs [path] (TimeRange Nothing Nothing) (Just $ start + items)
  let hist' = zip [(1 :: Int)..] hist
  toMaster <- getRouteToMaster
  let pageForwardLink = if length hist > items
                           then Just $ toMaster
                                     $ HistoryR (start + items) page
                           else Nothing
  let pageBackLink    = if start > 1
                           then Just $ toMaster
                                     $ HistoryR (start - items) page
                           else Nothing
  let tabs = if path == pagePath
                then [ViewTab,EditTab,HistoryTab,DiscussTab]
                else [ViewTab,HistoryTab]
  makePage pageLayout{ pgName = Just page
                     , pgTabs = tabs
                     , pgSelectedTab = HistoryTab } $ do
   addScript $ toMaster $ StaticR $ StaticRoute ["js","jquery-ui-1.8.21.custom.min.js"] []
   toWidget [julius|
      $(document).ready(function(){
          $(".difflink").draggable({helper: "clone"});
          $(".difflink").droppable({
               accept: ".difflink",
               drop: function(ev, ui) {
                  var targetOrder = parseInt($(this).attr("order"));
                  var sourceOrder = parseInt($(ui.draggable).attr("order"));
                  var diffurl = $(this).attr("diffurl");
                  if (targetOrder < sourceOrder) {
                      var fromRev = $(this).attr("revision");
                      var toRev   = $(ui.draggable).attr("revision");
                  } else {
                      var toRev   = $(this).attr("revision");
                      var fromRev = $(ui.draggable).attr("revision");
                  };
                  location.href = diffurl.replace('FROM',fromRev).replace('TO',toRev);
                  }
              });
      });
   |]
   [whamlet|
     <h1 .title>#{page}
     <p>_{MsgDragDiff}
     <ul>
       $forall (pos,rev) <- hist'
         <li .difflink order=#{pos} revision=#{revId rev} diffurl=@{toMaster $ DiffR "FROM" "TO" page}>
           ^{revisionDetails rev}
     ^{pagination pageBackLink pageForwardLink}
     |]

revisionDetails :: HasGitit master
                => Revision
                -> GWidget Gitit master ()
revisionDetails rev = do
  toMaster <- lift getRouteToMaster
  let toChange :: Change -> GHandler Gitit master (Text, Page)
      toChange (Modified f) = ("modified",) <$> pageForPath f
      toChange (Deleted  f) = ("deleted",)  <$> pageForPath f
      toChange (Added    f) = ("added",)    <$> pageForPath f
  changes <- lift $ mapM toChange $ revChanges rev
  [whamlet|
    <span .date>#{show $ revDateTime rev} 
    (<span .author>#{authorName $ revAuthor rev}</span>): 
    <span .subject>#{revDescription rev} 
    $forall (cls,pg) <- changes
      <a href=@{toMaster $ RevisionR (revId rev) pg}>
        <span .#{cls}>#{pg}</span> 
  |]

pagination :: HasGitit master
           => Maybe (Route master)    -- back link
           -> Maybe (Route master)    -- forward link
           -> GWidget Gitit master ()
pagination pageBackLink pageForwardLink =
   [whamlet|
     <p .pagination>
       $maybe bl <- pageBackLink
         <a href=@{bl}>&larr;
       &nbsp;
       $maybe fl <- pageForwardLink
         <a href=@{fl}>&rarr;
     |]

getActivityR :: HasGitit master
              => Int -> GHandler Gitit master RepHtml
getActivityR start = do
  let items = 20
  let offset = start - 1
  fs <- filestore <$> getYesodSub
  hist <- liftIO $ drop offset <$>
           history fs [] (TimeRange Nothing Nothing) (Just $ start + items)
  toMaster <- getRouteToMaster
  let pageForwardLink = if length hist > items
                           then Just $ toMaster
                                     $ ActivityR (start + items)
                           else Nothing
  let pageBackLink    = if start > 1
                           then Just $ toMaster
                                     $ ActivityR (start - items)
                           else Nothing
  makePage pageLayout{ pgName = Nothing
                     , pgTabs = []
                     , pgSelectedTab = HistoryTab } $ do
   [whamlet|
     <h1 .title>Recent activity
     <ul>
       $forall rev <- hist
         <li>
           ^{revisionDetails rev}
     ^{pagination pageBackLink pageForwardLink}
    |]

getAtomSiteR :: HasGitit master => GHandler Gitit master RepAtom
getAtomSiteR = feed Nothing >>= atomFeed

getAtomPageR :: HasGitit master => Page -> GHandler Gitit master RepAtom
getAtomPageR page = feed (Just page) >>= atomFeed

feed :: HasGitit master
     => Maybe Page  -- page, or nothing for all
     -> GHandler Gitit master (Feed (Route master))
feed mbpage = do
  days <- feed_days . config <$> getYesodSub
  toMaster <- getRouteToMaster
  mr <- getMessageRender
  fs <- filestore <$> getYesodSub
  now <- liftIO getCurrentTime
  paths <- case mbpage of
                Just p  -> (:[]) <$> pathForPage p
                Nothing -> return []
  let startTime = addUTCTime (fromIntegral $ -60 * 60 * 24 * days) now
  revs <- liftIO $ history fs paths
           TimeRange{timeFrom = Just startTime,timeTo = Nothing}
           (Just 200) -- hard limit of 200 to conserve resources
  let toEntry rev = do
        let topage change = case change of
                              Modified f -> ("" :: Text,) <$> pageForPath f
                              Deleted f  -> ("-",) <$> pageForPath f
                              Added f    -> ("+",) <$> pageForPath f
        firstpage <- case revChanges rev of
                           []    -> error "feed - encountered empty changes"
                           (c:_) -> snd <$> topage c
        let toChangeDesc c = do
             (m, pg) <- topage c
             return $ m <> pageToText pg
        changeDescrips <- mapM toChangeDesc $ revChanges rev
        return FeedEntry{
                   feedEntryLink    = toMaster $ RevisionR (revId rev) firstpage
                 , feedEntryUpdated = revDateTime rev
                 , feedEntryTitle   = T.intercalate ", " changeDescrips <> ": "
                                      <> T.pack (revDescription rev) <> " (" <>
                                      T.pack (authorName $ revAuthor rev) <> ")"
                 , feedEntryContent = toHtml $ T.pack ""
                 }
  entries <- mapM toEntry [rev | rev <- revs, not (null $ revChanges rev) ]
  return Feed{
        feedTitle = mr $ maybe MsgSiteFeedTitle MsgPageFeedTitle mbpage
      , feedLinkSelf = toMaster $ maybe AtomSiteR AtomPageR mbpage
      , feedLinkHome = toMaster HomeR
      , feedDescription = undefined -- only used for rss
      , feedLanguage = undefined    -- only used for rss
      , feedUpdated = now
      , feedEntries = entries
    }

postExportR :: HasGitit master
            => Page -> GHandler Gitit master (ContentType, Content)
postExportR page = do
  format <- runInputPost (ireq textField "format")
  case lookup format exportFormats of
         Nothing -> fail "Unrecognized format"
         Just f  -> do
           path <- pathForPage page
           mbcont <- getRawContents path Nothing
           case mbcont of
                Nothing   -> fail "Could not get page contents"
                Just cont -> contentsToWikiPage page cont >>= f

-- TODO:
-- fix mime types
-- handle math in html formats
-- other slide show issues (e.g. dzslides core)
-- add pdf, docx, odt, epub
exportFormats :: [(Text, WikiPage -> GHandler Gitit master (ContentType, Content))]
exportFormats =
  [ ("Groff man", basicExport "man" ".1" typePlain writeMan)
  , ("reStructuredText", basicExport "rst" ".txt" typePlain writeRST)
  , ("Markdown", basicExport "markdown" ".txt" typePlain writeMarkdown)
  , ("Textile", basicExport "textile" ".textile" typePlain writeTextile)
  , ("Plain text", basicExport "plain" ".txt" typePlain writePlain)
  , ("Org-mode", basicExport "org" ".org" typePlain writeOrg)
  , ("Asciidoc", basicExport "asciidoc" ".txt" typePlain writeAsciiDoc)
  , ("Mediawiki", basicExport "mediawiki" ".wiki" typePlain writeMediaWiki)
  , ("HTML", basicExport "html" ".html" typeHtml writeHtmlString)
  , ("HTML5", basicExport "html5" ".html" typeHtml $ \opts ->
              writeHtmlString opts{ writerHtml5 = True })
  , ("S5", basicExport "s5" ".html" typeHtml $ \opts ->
              writeHtmlString opts{ writerSlideVariant = S5Slides })
  , ("Slidy", basicExport "slidy" ".html" typeHtml $ \opts ->
              writeHtmlString opts{ writerSlideVariant = SlidySlides })
  , ("DZSlides", basicExport "dzslides" ".html" typeHtml $ \opts ->
              writeHtmlString opts{ writerSlideVariant = DZSlides
                            , writerHtml5 = True })
  , ("LaTeX", basicExport "latex" ".tex" "application/x-latex" writeLaTeX)
  , ("Beamer", basicExport "beamer" ".tex" "application/x-latex" writeLaTeX)
  , ("ConTeXt", basicExport "context" ".tex" "application/x-context" writeConTeXt)
  , ("DocBook", basicExport "docbook" ".xml" "application/docbook+xml" writeDocbook)
  , ("OpenDocument", basicExport "opendocument" ".xml" "application/vnd.oasis.opendocument.text" writeOpenDocument)
  , ("Texinfo", basicExport "texinfo" ".texi" "application/x-texinfo" writeTexinfo)
  , ("RTF", basicExport "rtf" ".rtf" "application/rtf" writeRTF)
  ]

basicExport :: String -> Text -> ContentType -> (WriterOptions -> Pandoc -> String)
            -> WikiPage -> GHandler Gitit master (ContentType, Content)
basicExport templ extension contentType writer = \wikiPage -> do
  setFilename $ wpName wikiPage <> extension
  conf <- config <$> getYesodSub
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
                  dztempl <- readDataFile (pandoc_user_data conf)
                             $ "dzslides" </> "template.html"
                  return $ unlines
                      $ dropWhile (not . isPrefixOf "<!-- {{{{ dzslides core")
                      $ lines dztempl
                else return ""
  let rendered =  writer defaultWriterOptions{
                                  writerTemplate = template
                                , writerSourceDirectory = wiki_path conf
                                , writerStandalone = True
                                , writerLiterateHaskell = wpLHS wikiPage
                                , writerTableOfContents = wpTOC wikiPage
                                , writerHTMLMathMethod = MathML Nothing
                                , writerVariables = ("dzslides-core",dzcore):vars }
        $ Pandoc (Meta (wpTitle wikiPage) [] []) $ wpContent wikiPage
  rendered' <- if contentType == typeHtml
                  then liftIO $ inDirectory (wiki_path conf)
                       $ makeSelfContained (pandoc_user_data conf) rendered
                  else return rendered
  sendResponse (contentType, toContent rendered')

setFilename :: Text -> GHandler sub master ()
setFilename fname = setHeader "Content-Disposition"
                  $ "attachment; filename=\"" <> fname <> "\""

getUploadR :: HasGitit master => GHandler Gitit master RepHtml
getUploadR = undefined

postUploadR :: HasGitit master => GHandler Gitit master RepHtml
postUploadR = undefined

----------
-- Caching
----------

postExpireR :: HasGitit master => Page -> GHandler Gitit master RepHtml
postExpireR page = do
  useCache <- use_cache . config <$> getYesodSub
  if useCache
     then do
       path <- pathForPage page
       path' <- pathForFile page
       expireCache path
       expireCache path'
     else return ()
  toMaster <- getRouteToMaster
  redirect $ toMaster $ ViewR page

caching :: FilePath -> GHandler Gitit master RepHtml -> GHandler Gitit master RepHtml
caching path handler = do
  cachedir <- cache_dir . config <$> getYesodSub
  let fullpath = cachedir </> path
  RepHtml contents <- handler
  case contents of
       ContentBuilder builder _ -> liftIO $ do
         createDirectoryIfMissing True $ takeDirectory fullpath
         toByteStringIO (BS.writeFile fullpath) builder
         return $ RepHtml contents
       _ -> return $ RepHtml contents

lookupCache :: FilePath -> GHandler Gitit master (Maybe FilePath)
lookupCache path = do
  cachedir <- cache_dir . config <$> getYesodSub
  let fullpath = cachedir </> path
  exists <- liftIO $ doesFileExist fullpath
  return $ if exists
              then Just fullpath
              else Nothing

expireCache :: FilePath -> GHandler Gitit master ()
expireCache path = do
  cachedir <- cache_dir . config <$> getYesodSub
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesFileExist fullpath
    when exists $ removeFile $ cachedir </> path
    -- TODO also expire any exports based on this page
    -- ultimately we'll cache exports in a special directory
    -- based on the path of the page's source, e.g.
    -- "Front Page.page.d/Front Page.html"
