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

import           Control.Exception (catch, throw, try)
import           Control.Monad (filterM, mplus, when, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Lazy (fromChunks)
import           Data.ByteString.Lazy.UTF8 (toString)
import           Data.Char (toLower)
import           Data.Conduit (($$))
import           Data.Conduit.List (consume)
import           Data.FileStore as FS
import           Data.List (find, inits, intercalate, nub, sort, sortBy)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Text as T
import           Data.Time (getCurrentTime, addUTCTime)
import           Data.Time.Clock (diffUTCTime)
import           Data.Yaml
import           Network.Gitit2.Cache
import           Network.Gitit2.Handler.History
import           Network.Gitit2.Handler.View
import           Network.Gitit2.Import
import           Network.Gitit2.Page
import           Network.Gitit2.WikiPage (extractCategories, readPageFormat)
import           System.Directory
import           System.FilePath
import           System.IO (Handle, withFile, IOMode(..))
import           System.IO.Error (isEOFError)
import           System.Random (randomRIO)
import           Yesod.AtomFeed
import           Yesod.Static

instance HasGitit master => YesodSubDispatch Gitit (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesGitit)

data HtmlMathMethod = UseMathML | UseMathJax | UsePlainMath

-- HANDLERS and utility functions, not exported:

-- pathForFile :: Page -> GH master FilePath
-- pathForFile p = return $ T.unpack $ toMessage p

isPageFile :: FilePath -> GH master Bool
isPageFile f = do
  conf <- getConfig
  return $ takeExtension f == page_extension conf

allPageFiles :: GH master [FilePath]
allPageFiles = do
  fs <- filestore <$> getYesod
  liftIO (index fs) >>= filterM isPageFile

isDiscussPageFile :: FilePath -> GH master Bool
isDiscussPageFile ('@':xs) = isPageFile xs
isDiscussPageFile _ = return False

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

getRandomR :: HasGitit master => GH master Html
getRandomR = do
  fs <- filestore <$> getYesod
  files <- liftIO $ index fs
  pages <- mapM pageForPath =<< filterM (fmap not . isDiscussPageFile)
                            =<<filterM isPageFile files
  pagenum <- liftIO $ randomRIO (0, length pages - 1)
  let thepage = pages !! pagenum
  redirect $ ViewR thepage

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

getDeleteR :: HasGitit master => Page -> GH master Html
getDeleteR page = do
  requireUser
  fs <- filestore <$> getYesod
  path <- pathForPage page
  pageTest <- liftIO $ try $ latest fs path
  fileToDelete <- case pageTest of
                       Right _        -> return path
                       Left  FS.NotFound -> do
                         let path' = pathForFile page
                         fileTest <- liftIO $ try $ latest fs path'
                         case fileTest of
                              Right _     -> return path' -- a file
                              Left FS.NotFound  -> fail (show FS.NotFound)
                              Left e      -> fail (show e)
                       Left e        -> fail (show e)
  toMaster <- getRouteToParent
  makePage pageLayout{ pgName = Just page
                     , pgTabs = []
                     }
    [whamlet|
      <h1>#{page}</h1>
      <div #deleteform>
        <form method=post action=@{toMaster $ DeleteR page}>
          <p>_{MsgConfirmDelete page}
          <input type=text class=hidden name=fileToDelete value=#{fileToDelete}>
          <input type=submit value=_{MsgDelete}>
    |]

postDeleteR :: HasGitit master => Page -> GH master Html
postDeleteR page = do
  user <- requireUser
  fs <- filestore <$> getYesod
  mr <- getMessageRender
  fileToDelete <- lift $ runInputPost $ ireq textField "fileToDelete"
  liftIO $ FS.delete fs (T.unpack fileToDelete)
            (Author (gititUserName user) (gititUserEmail user))
            (T.unpack $ mr $ MsgDeleted page)
  setMessageI $ MsgDeleted page
  redirect HomeR

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


getDiffR :: HasGitit master
         => RevisionId -> RevisionId -> Page -> GH master Html
getDiffR fromRev toRev page = do
  fs <- filestore <$> getYesod
  pagePath <- pathForPage page
  let filePath = pathForFile page
  rawDiff <- liftIO
             $ catch (diff fs pagePath (Just fromRev) (Just toRev))
             $ \e -> case e of
                      FS.NotFound -> diff fs filePath (Just fromRev) (Just toRev)
                      _           -> throw e
  makePage pageLayout{ pgName = Just page
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $
   [whamlet|
     <h1 .title>#{page}
     <h2 .revision>#{fromRev} &rarr; #{toRev}
     <pre>
        $forall t <- rawDiff
           $case t
             $of Both xs _
               <span .unchanged>#{intercalate "\n" xs}
             $of First xs
               <span .deleted>#{intercalate "\n" xs}
             $of Second xs
               <span .added>#{intercalate "\n" xs}
     |]

getAtomSiteR :: HasGitit master => GH master RepAtom
getAtomSiteR = do
  tryCache "_feed"
  caching "_feed" $ feed Nothing >>= atomFeed

getAtomPageR :: HasGitit master => Page -> GH master RepAtom
getAtomPageR page = do
  path <- pathForPage page
  tryCache (path </> "_feed")
  caching (path </> "_feed") $ feed (Just page) >>= atomFeed

feed :: HasGitit master
     => Maybe Page  -- page, or nothing for all
     -> GH master (Feed (Route Gitit))
feed mbpage = do
  days <- feed_days <$> getConfig
  mr <- getMessageRender
  fs <- filestore <$> getYesod
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
                   feedEntryLink    = RevisionR (revId rev) firstpage
                 , feedEntryUpdated = revDateTime rev
                 , feedEntryTitle   = T.intercalate ", " changeDescrips <> ": "
                                      <> T.pack (revDescription rev) <> " (" <>
                                      T.pack (authorName $ revAuthor rev) <> ")"
                 , feedEntryContent = toHtml $ T.pack ""
                 }
  entries <- mapM toEntry [rev | rev <- revs, not (null $ revChanges rev) ]
  return Feed{
        feedAuthor = ""
      , feedTitle = mr $ maybe MsgSiteFeedTitle MsgPageFeedTitle mbpage
      , feedLinkSelf = maybe AtomSiteR AtomPageR mbpage
      , feedLinkHome = HomeR
      , feedDescription = undefined -- only used for rss
      , feedLanguage = undefined    -- only used for rss
      , feedUpdated = now
      , feedEntries = entries
    }

-- TODO:
-- fix mime types
-- handle math in html formats
-- other slide show issues (e.g. dzslides core)
-- add pdf, docx, odt, epu

getUploadR :: HasGitit master => GH master Html
getUploadR = do
  requireUser
  (form, enctype) <- lift $ generateFormPost $ uploadForm Nothing
  showUploadForm enctype form

showUploadForm :: HasGitit master
               => Enctype
               -> WidgetT master IO ()
               -> GH master Html
showUploadForm enctype form = do
  toMaster <- getRouteToParent
  makePage pageLayout{ pgName = Nothing
                     , pgTabs = []
                     , pgSelectedTab = EditTab } $ do
    toWidget $ [julius|
      $(document).ready(function(){
          $("#file").change(function () {
            var fn = $(this).val().replace(/.*\\/,"");
            $("#wikiname").val(fn);
          });
        });
    |]
    [whamlet|
      <h1>_{MsgUploadFile}</h1>
      <div #uploadform>
        <form method=post action=@{toMaster UploadR} enctype=#{enctype}>
          ^{form}
          <input type=submit>
    |]

data Upload = Upload { uploadFile        :: FileInfo
                     , uploadWikiname    :: Text
                     , uploadDescription :: Text
                     , uploadOverwrite   :: Bool
                     }

uploadForm :: HasGitit master
           => Maybe Upload
           -> Html
           -> MForm (HandlerT master IO) (FormResult Upload, WidgetT master IO ())
uploadForm mbupload =
  renderDivs $ Upload
     <$> fileAFormReq (fieldSettingsLabel MsgFileToUpload){
                fsId = Just "file" }
     <*> areq pageField (fieldSettingsLabel MsgWikiName){
                fsId = Just "wikiname" } (uploadWikiname <$> mbupload)
     <*> areq commentField (fieldSettingsLabel MsgChangeDescription)
            (uploadDescription <$> mbupload)
     <*> areq checkBoxField (fieldSettingsLabel MsgOverwrite)
            (uploadOverwrite <$> mbupload)
   where commentField = check validateNonempty textField
         validateNonempty y
           | T.null y = Left MsgValueRequired
           | otherwise = Right y
         pageField = check validatePage textField
         validatePage x = case fromPathMultiPiece (T.splitOn "/" x) of
                                Just (_ :: Page) -> Right x
                                Nothing          -> Left MsgInvalidPageName


postUploadR :: HasGitit master => GH master Html
postUploadR = do
  user <- requireUser
  ((result, widget), enctype) <- lift $ runFormPost $ uploadForm Nothing
  fs <- filestore <$> getYesod
  case result of
       FormSuccess r -> do
         let fileinfo = uploadFile r
         let page = textToPage $ uploadWikiname r
         let auth = Author (gititUserName user) (gititUserEmail user)
         let comm = T.unpack $ uploadDescription r
         let path = pathForFile page
         allfiles <- liftIO $ index fs
         if path `elem` allfiles && not (uploadOverwrite r)
            then do
              setMessageI MsgFileExists
              showUploadForm enctype widget
            else do
              cont <- lift $ fromChunks <$> (fileSource fileinfo $$ consume)
              res <- liftIO $ try $ save fs path auth comm cont
              case res of
                   Left FS.Unchanged -> do
                                        setMessageI MsgFileUnchanged
                                        showUploadForm enctype widget
                   Left e            -> throw e
                   Right _           -> do
                                        expireCache path
                                        redirect $ ViewR page
       _             -> showUploadForm enctype widget

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

expireCache :: FilePath -> GH master ()
expireCache path = do
  conf <- getConfig
  expireFeed (feed_minutes conf) (path </> "_feed")
  expireFeed (feed_minutes conf) "_feed"
  expireCategories
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ removeDirectoryRecursive fullpath

expireCategories :: GH master ()
expireCategories = do
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> "_categories"
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ removeDirectoryRecursive fullpath

-- | Expire the cached feed unless it is younger than 'minutes' old.
expireFeed :: Integer -> FilePath -> GH master ()
expireFeed minutes path = do
  cachedir <- cache_dir <$> getConfig
  let fullpath = cachedir </> path
  liftIO $ do
    exists <- doesDirectoryExist fullpath
    when exists $ do
      seconds <- getModificationTime fullpath
      seconds' <- getCurrentTime
      unless (diffUTCTime seconds' seconds < realToFrac (minutes * 60))
        $ removeDirectoryRecursive fullpath

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



