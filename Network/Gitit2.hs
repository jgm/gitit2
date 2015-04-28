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

import           Control.Exception (catch, throw)
import           Control.Monad (filterM, mplus, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Char (toLower)
import           Data.FileStore as FS
import           Data.List (find, inits, nub, sort, sortBy)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Data.Yaml
import           Network.Gitit2.Cache
import           Network.Gitit2.Handler.Atom
import           Network.Gitit2.Handler.Delete
import           Network.Gitit2.Handler.Diff
import           Network.Gitit2.Handler.History
import           Network.Gitit2.Handler.Random
import           Network.Gitit2.Handler.Upload
import           Network.Gitit2.Handler.View
import           Network.Gitit2.Import
import           Network.Gitit2.Page
import           Network.Gitit2.WikiPage (extractCategories, readPageFormat)
import           System.FilePath
import           System.IO (Handle, withFile, IOMode(..))
import           System.IO.Error (isEOFError)
import           Yesod.Static

instance HasGitit master => YesodSubDispatch Gitit (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesGitit)

data HtmlMathMethod = UseMathML | UseMathJax | UsePlainMath

-- HANDLERS and utility functions, not exported:

-- pathForFile :: Page -> GH master FilePath
-- pathForFile p = return $ T.unpack $ toMessage p

allPageFiles :: GH master [FilePath]
allPageFiles = do
  fs <- filestore <$> getYesod
  liftIO (index fs) >>= filterM isPageFile

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

postSearchR :: HasGitit master => GH master Html
postSearchR = do
  patterns <- lift $ runInputPost $ ireq textField "patterns"
  searchResults $ T.words patterns

postGoR :: HasGitit master => GH master Html
postGoR = do
  gotopage <- lift $ runInputPost $ ireq textField "gotopage"
  let gotopage' = T.toLower gotopage
  allPages <- allPageFiles
  let allPageNames = map (T.pack . dropExtension) allPages
  let findPage f   = find f allPageNames
  let exactMatch f = gotopage == f
  let insensitiveMatch f = gotopage' == T.toLower f
  let prefixMatch f = gotopage' `T.isPrefixOf` T.toLower f
  case findPage exactMatch `mplus` findPage insensitiveMatch `mplus`
        findPage prefixMatch of
       Just m  -> redirect $ ViewR $ textToPage m
       Nothing -> searchResults $ T.words gotopage

searchResults :: HasGitit master => [Text] -> GH master Html
searchResults patterns = do
  fs <- filestore <$> getYesod
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
  let inPageName pageName' x = x `elem` words
           (slashToSpace $ dropExtension pageName')
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
  let matches' = sortBy (flip (comparing relevance)) matches
  let matches'' = map (\(f,c) -> (textToPage $ T.pack $ dropExtension f, c)) matches'
  toMaster <- getRouteToParent
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

-- categories ------------

-- NOTE:  The current implementation of of categories does not go via the
-- filestore abstraction.  That is bad, but can only be fixed if we add
-- more sophisticated searching options to filestore.

getCategoriesR :: HasGitit master => GH master Html
getCategoriesR = do
  tryCache "_categories"
  conf <- getConfig
  toMaster <- getRouteToParent
  let repopath = repository_path conf
  allpages <- map (repopath </>) <$> allPageFiles
  allcategories <- liftIO $ nub . sort . concat <$> mapM readCategories allpages
  caching "_categories" $
    makePage pageLayout{ pgName = Nothing
                       , pgTabs = []
                       , pgSelectedTab = EditTab }
    [whamlet|
      <h1>_{MsgCategories}</h1>
      <ul>
        $forall category <- allcategories
          <li><a href=@{toMaster $ CategoryR category}>#{category}
    |]

getCategoryR :: HasGitit master => Text -> GH master Html
getCategoryR category = do
  let cachepage = "_categories" </> T.unpack category
  tryCache cachepage
  conf <- getConfig
  toMaster <- getRouteToParent
  let repopath = repository_path conf
  allpages <- allPageFiles
  let hasCategory pg = elem category <$> readCategories (repopath </> pg)
  matchingpages <- mapM pageForPath =<< (sort <$> filterM (liftIO . hasCategory) allpages)
  caching cachepage $
    makePage pageLayout{ pgName = Nothing
                       , pgTabs = []
                       , pgSelectedTab = EditTab }
    [whamlet|
      <h1>_{MsgCategory}: #{category}</h1>
      <ul.index>
        $forall page <- matchingpages
          <li .page><a href=@{toMaster $ ViewR page}>#{page}
    |]

-- | Examine metadata at beginning of file, returning list of categories.
-- Note:  Must be strict.
readCategories :: FilePath -> IO [Text]
readCategories f = do
  hdr <- getHeader f
  return $ if BS.null hdr
     then []
     else extractCategories $ fromMaybe M.empty $ decode hdr

getHeader :: FilePath -> IO BS.ByteString
getHeader f =
  withFile f ReadMode $ \h ->
    catch (do fl <- BS.hGetLine h
              if dashline fl
                 then BSC.unlines <$> hGetLinesTill h dotline
                 else return BS.empty)
       (\e -> if isEOFError e then return BS.empty else throw e)

dashline :: BS.ByteString -> Bool
dashline x =
  case BSC.unpack x of
       ('-':'-':'-':xs) | all (==' ') xs -> True
       _ -> False

dotline :: BS.ByteString -> Bool
dotline x =
  case BSC.unpack x of
       ('.':'.':'.':xs) | all (==' ') xs -> True
       _ -> False

hGetLinesTill :: Handle -> (BS.ByteString -> Bool) -> IO [BS.ByteString]
hGetLinesTill h end = do
  next <- BS.hGetLine h
  if end next
     then return []
     else do
       rest <- hGetLinesTill h end
       return (next:rest)



