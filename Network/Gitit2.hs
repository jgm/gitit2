{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleInstances,
             FlexibleContexts, ScopedTypeVariables, TupleSections,
             ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.Gitit2 ( GititConfig (..)
                      , HtmlMathMethod (..)
                      , Page (..)
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
                      , Plugin (..)
                      ) where

import           Control.Monad (filterM,  when)
import           Data.FileStore as FS
import           Data.List (inits)
import qualified Data.Text as T
import           Network.Gitit2.Cache
import           Network.Gitit2.Handler.Atom
import           Network.Gitit2.Handler.Category
import           Network.Gitit2.Handler.Delete
import           Network.Gitit2.Handler.Diff
import           Network.Gitit2.Handler.Edit
import           Network.Gitit2.Handler.History
import           Network.Gitit2.Handler.Random
import           Network.Gitit2.Handler.Search
import           Network.Gitit2.Handler.Upload
import           Network.Gitit2.Handler.View
import           Network.Gitit2.Import
import           Network.Gitit2.Page
import           Network.Gitit2.WikiPage (readPageFormat)
import           System.FilePath
import           Yesod.Static

instance HasGitit master => YesodSubDispatch Gitit (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesGitit)

data HtmlMathMethod = UseMathML | UseMathJax | UsePlainMath

-- HANDLERS and utility functions, not exported:

-- pathForFile :: Page -> GH master FilePath
-- pathForFile p = return $ T.unpack $ toMessage p

getGititRobotsR :: GH m RepPlain
getGititRobotsR = do
  conf <- getConfig
  sendFile "text/plain" (static_path conf </> "robots.txt")

getGititFaviconR :: GH m ()
getGititFaviconR = do
  conf <- getConfig
  sendFile "image/x-icon" (static_path conf </> "favicon.ico")

getHomeR :: HasGitit master => GH master ()
getHomeR = do
  conf <- getConfig
  redirect $ ViewR $ textToPage $ front_page conf

getHelpR :: HasGitit master => GH master Html
getHelpR = do
  conf <- getConfig
  getViewR $ textToPage $ help_page conf

getRawR :: HasGitit master => Page -> GH master RepPlain
getRawR page = do
  path <- pathForPage page
  mbcont <- getRawContents path Nothing
  case mbcont of
       Nothing       -> do
         let path' = pathForFile page
         mbcont' <- getRawContents path' Nothing
         case mbcont' of
              Nothing   -> notFound
              Just cont -> return $ RepPlain $ toContent cont
       Just cont -> return $ RepPlain $ toContent cont

postPreviewR :: HasGitit master => GH master Html
postPreviewR =
  undefined -- TODO: get raw contents and settings from post params
  -- return HTML for rendered page contents
  -- a javascript gizmo will display this in a modal or something
  -- factor out some of the code from view

getIndexBaseR :: HasGitit master => GH master Html
getIndexBaseR = getIndexFor []

getIndexR :: HasGitit master => Page -> GH master Html
getIndexR (Page xs) = getIndexFor xs

getIndexFor :: HasGitit master => [Text] -> GH master Html
getIndexFor paths = do
  fs <- filestore <$> getYesod
  listing <- liftIO $ directory fs $ T.unpack $ T.intercalate "/" paths
  let isDiscussionPage (FSFile f) = isDiscussPageFile f
      isDiscussionPage (FSDirectory _) = return False
  prunedListing <- filterM (fmap not . isDiscussionPage) listing
  let updirs = inits $ filter (not . T.null) paths
  toMaster <- getRouteToParent
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

upDir :: (Route Gitit -> Route master) -> [Text] -> WidgetT master IO ()
upDir toMaster fs = do
  let (route, lastdir) = case reverse fs of
                          (f:_)  -> (IndexR $ Page fs, f)
                          []     -> (IndexBaseR, "\x2302")
  [whamlet|<a href=@{toMaster $ route}>#{lastdir}/</a>|]


-- TODO:
-- fix mime types
-- handle math in html formats
-- other slide show issues (e.g. dzslides core)
-- add pdf, docx, odt, epu


----------
-- Caching
--
-- We cache Blah.page as Blah.page/_page.html, and any of its exports
-- as Blah.page/EXPORT_FORMAT/filename.  Remove the whole Blah.page directory
-- expires all of them.  Non-pages Foo.jpg just get cached as Foo.jpg.
----------

postExpireHomeR :: HasGitit master => GH master Html
postExpireHomeR = do
  conf <- getConfig
  postExpireR $ textToPage $ front_page conf

postExpireR :: HasGitit master => Page -> GH master Html
postExpireR page = do
  useCache <- use_cache <$> getConfig
  when useCache $
     do
       pathForPage page >>= expireCache
       expireCache $ pathForFile page
  redirect $ ViewR page
