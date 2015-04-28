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
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.FileStore as FS
import           Data.List (inits)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.Gitit2.Cache
import           Network.Gitit2.Handler.Atom
import           Network.Gitit2.Handler.Category
import           Network.Gitit2.Handler.Delete
import           Network.Gitit2.Handler.Diff
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

getEditR :: HasGitit master => Page -> GH master Html
getEditR page = do
  requireUser
  fs <- filestore <$> getYesod
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
           => RevisionId -> Page -> GH master Html
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
     -> GH master Html
edit revert txt mbrevid page = do
  requireUser
  let contents = Textarea $ T.pack txt
  mr <- getMessageRender
  let comment = if revert
                   then mr $ MsgReverted $ fromMaybe "" mbrevid
                   else ""
  (form, enctype) <- lift $ generateFormPost $ editForm
                     $ Just Edit{ editContents = contents
                                , editComment = comment }
  toMaster <- getRouteToParent
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
             -> WidgetT master IO ()
             -> GH master Html
showEditForm page route enctype form =
  makePage pageLayout{ pgName = Just page
                     , pgTabs = [EditTab]
                     , pgSelectedTab = EditTab }
    [whamlet|
      <h1>#{page}</h1>
      <div #editform>
        <form method=post action=@{route} enctype=#{enctype}>
          ^{form}
          <input type=submit>
    |]

postUpdateR :: HasGitit master
          => RevisionId -> Page -> GH master Html
postUpdateR revid = update' (Just revid)

postCreateR :: HasGitit master
            => Page -> GH master Html
postCreateR = update' Nothing

update' :: HasGitit master
       => Maybe RevisionId -> Page -> GH master Html
update' mbrevid page = do
  user <- requireUser
  ((result, widget), enctype) <- lift $ runFormPost $ editForm Nothing
  fs <- filestore <$> getYesod
  toMaster <- getRouteToParent
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
                      redirect $ ViewR page
                   Left mergeinfo -> do
                      setMessageI $ MsgMerged revid
                      edit False (mergeText mergeinfo)
                           (Just $ revId $ mergeRevision mergeinfo) page
           Nothing -> do
             expireCache path
             liftIO $ save fs path auth comm cont
             redirect $ ViewR page
       _ -> showEditForm page route enctype widget

data Edit = Edit { editContents :: Textarea
                 , editComment  :: Text
                 } deriving Show

editForm :: HasGitit master
         => Maybe Edit
         -> Html
         -> MForm (HandlerT master IO) (FormResult Edit, WidgetT master IO ())
editForm mbedit = renderDivs $ Edit
    <$> areq textareaField (fieldSettingsLabel MsgPageSource)
           (editContents <$> mbedit)
    <*> areq commentField (fieldSettingsLabel MsgChangeDescription)
           (editComment <$> mbedit)
  where commentField = check validateNonempty textField
        validateNonempty y
          | T.null y = Left MsgValueRequired
          | otherwise = Right y

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
