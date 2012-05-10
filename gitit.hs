{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Yesod.Static
import Yesod.Default.Handlers -- robots, favicon
import Data.Monoid (Monoid (mappend, mempty, mconcat), (<>))
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (isInfixOf, inits)
import Data.Text (Text)
import Data.FileStore
import System.FilePath
import Text.Pandoc
import Control.Applicative
import qualified Data.Text as T
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Text.Blaze.Html

data Config = Config{ wiki_root  :: Text
                    , wiki_path  :: FilePath
                    , static_dir :: FilePath
                    }

data Page = Page Text deriving (Show, Read, Eq)

instance PathMultiPiece Page where
  toPathMultiPiece (Page x) = T.splitOn "/" x
  fromPathMultiPiece (x:xs) = if "_" `T.isPrefixOf` x
                              then Nothing
                              else Just (Page $ T.intercalate "/" $ x:xs)
  fromPathMultiPiece []     = Nothing

instance ToMarkup Page where
  toMarkup (Page x) = toMarkup x

data Dir = Dir Text deriving (Show, Read, Eq)

instance PathMultiPiece Dir where
  toPathMultiPiece (Dir x) = T.splitOn "/" x
  fromPathMultiPiece (x:xs) = if "_" `T.isPrefixOf` x
                              then Nothing
                              else Just (Dir $ T.intercalate "/" $ x:xs)
  fromPathMultiPiece []     = Just $ Dir ""

instance ToMarkup Dir where
  toMarkup (Dir x) = toMarkup x

defaultConfig :: Config
defaultConfig = Config{ wiki_root  = ""
                      , wiki_path  = "wikidata"
                      , static_dir = "public"
                      }

data Gitit = Gitit{ settings      :: Config
                  , filestore     :: FileStore
                  , getStatic     :: Static
                  }

mkYesod "Gitit" [parseRoutesNoCheck|
/ HomeR GET
/_static StaticR Static getStatic
/_index/*Dir  IndexR GET
/favicon.ico FaviconR GET
/robots.txt RobotsR GET
/_edit/*Page  EditR GET POST
/*Page     ViewR GET
|]

instance Yesod Gitit where
  approot = ApprootMaster $ wiki_root . settings
  defaultLayout contents = do
    PageContent title headTags bodyTags <- widgetToPageContent $ do
      addStylesheet $ StaticR $ StaticRoute ["css","custom.css"] []
      addScript $ StaticR $ StaticRoute ["js","jquery-1.7.2.min.js"] []
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
               <div #message>#{msg}
             ^{bodyTags}
        |]

type Form x = Html -> MForm Gitit Gitit (FormResult x, Widget)

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Gitit FormMessage where
    renderMessage _ _ = defaultFormMessage

pageLayout :: Maybe Page -> Widget -> Handler RepHtml
pageLayout mbpage content = do
  defaultLayout [whamlet|
    <div #doc3 class="yui-t1">
      <div #yui-main>
        <div #maincol class="yui-b">
          ^{content}
      <div #sidebar class="yui-b first">
        <div #logo>
          <img src="/_static/img/logo.png" alt="logo">
        <div class="sitenav">
          sitenav
          $maybe page <- mbpage
            pagecontrols for #{page}
  |]

pathForPage :: Page -> FilePath
pathForPage (Page page) = T.unpack page <.> "page"

pageForPath :: FilePath -> Page
pageForPath fp = Page . T.pack $
  if isPageFile fp then dropExtension fp else fp

isPage :: String -> Bool
isPage "" = False
isPage ('_':_) = False
isPage s = all (`notElem` "*?") s && not (".." `isInfixOf` s) && not ("/_" `isInfixOf` s)
-- for now, we disallow @*@ and @?@ in page names, because git filestore
-- does not deal with them properly, and darcs filestore disallows them.

isPageFile :: FilePath -> Bool
isPageFile f = takeExtension f == ".page"

isDiscussPage :: String -> Bool
isDiscussPage ('@':xs) = isPage xs
isDiscussPage _ = False

isDiscussPageFile :: FilePath -> Bool
isDiscussPageFile ('@':xs) = isPageFile xs
isDiscussPageFile _ = False

getHomeR :: Handler RepHtml
getHomeR = getViewR (Page "Front Page")

getViewR :: Page -> Handler RepHtml
getViewR page = do
  contents <- getRawContents page Nothing
  pageLayout (Just page) $ [whamlet|
    <h1 class="title">#{page}
    ^{htmlPage contents}
  |]

getIndexR :: Dir -> Handler RepHtml
getIndexR (Dir dir) = do
  fs <- filestore <$> getYesod
  listing <- liftIO $ directory fs $ T.unpack dir
  let isDiscussionPage (FSFile f) = isDiscussPageFile f
      isDiscussionPage (FSDirectory _) = False
  let prunedListing = filter (not . isDiscussionPage) listing
  let updirs = inits $ filter (not . T.null) $ toPathMultiPiece (Dir dir)
  pageLayout Nothing $ [whamlet|
    <h1 class="title">
      $forall up <- updirs
        ^{upDir up}
    <div class="index">
      <ul>
        $forall ent <- prunedListing
          ^{indexListing dir ent}
  |]

upDir :: [Text] -> Widget
upDir fs = do
  let lastdir = case reverse fs of
                     (f:_)  -> f
                     []     -> "[root]"
  [whamlet|<a href="@{IndexR $ maybe (Dir "") id $ fromPathMultiPiece fs}">#{lastdir}/</a>|]

indexListing :: Text -> Resource -> Widget
indexListing dir r = do
  let pref = if T.null dir
                then ""
                else dir <> "/"
  let fullName f = pref <> f'
                     where Page f' = pageForPath f
  case r of
    (FSFile f) ->
       let cls :: Text
           cls = if isPageFile f then "page" else "upload"
       in  [whamlet|
          <li class="#{cls}">
            <a href="@{ViewR $ Page $ fullName f}">#{fullName f}</a>
          |]
    (FSDirectory f) -> [whamlet|
          <li class="folder">
            <a href="@{IndexR $ Dir $ fullName f}">#{fullName f}</a>
          |]

getRawContents :: Page -> Maybe RevisionId -> Handler ByteString
getRawContents page rev = do
  fs <- filestore <$> getYesod
  liftIO $ retrieve fs (pathForPage page) rev

htmlPage :: ByteString -> Widget
htmlPage contents = do
  let mathjax_url = "https://d3eoax9i5htok0.cloudfront.net/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  let rendered = writeHtml defaultWriterOptions{
                     writerWrapText = False
                   , writerHtml5 = True
                   , writerHighlight = True
                   , writerHTMLMathMethod = MathJax $ T.unpack mathjax_url }
                   $ readMarkdown defaultParserState{
                      stateSmart = True }
                   $ toString contents
  addScriptRemote mathjax_url
  toWidget rendered

getEditR :: Page -> Handler RepHtml
getEditR page = do
  contents <- Textarea . T.pack . toString <$> getRawContents page Nothing
  (form, enctype) <- generateFormPost $ editForm $ Just Edit{ editContents = contents, editComment = "" }
  pageLayout (Just page) $ do
    toWidget [lucius|
      textarea { width: 45em; height: 20em; font-family: monospace; }
      input[type='text'] { width: 45em; } 
      label { display: block; font-weight: bold; font-size: 80%; font-family: sans-serif; }
    |]
    [whamlet|
      <h1>#{page}</h1>
      <form method=post action=@{EditR page} enctype=#{enctype}>
        ^{form}
        <input type=submit>
    |]

postEditR :: Page -> Handler RepHtml
postEditR page = do
  ((res, form), enctype) <- runFormPost $ editForm Nothing
  fs <- filestore <$> getYesod
  case res of
       FormSuccess r -> do
          liftIO $ modify fs (pathForPage page) ""
            (Author "Dummy" "me@somewhere.net")
            (T.unpack $ editComment r) (filter (/='\r') . T.unpack $ unTextarea $ editContents r)
          -- TODO handle mergeinfo
          return ()
       _             -> return ()
  getViewR page

data Edit = Edit { editContents :: Textarea
                 , editComment  :: Text
                 } deriving Show

editForm :: Maybe Edit-> Form Edit
editForm mbedit = renderDivs $ Edit
    <$> areq textareaField "Text of page" (editContents <$> mbedit)
    <*> areq commentField "Change description" (editComment <$> mbedit)
  where errorMessage :: Text
        errorMessage = "Comment can't be empty"
        commentField = check validateNonempty textField
        validateNonempty y
          | T.null y = Left errorMessage
          | otherwise = Right y


main :: IO ()
main = do
  let conf = defaultConfig
  let fs = gitFileStore $ wiki_path conf
  st <- staticDevel "static"
  warpDebug 3000 (Gitit{ settings = conf
                       , filestore = fs
                       , getStatic = st
                       })


