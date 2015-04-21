module Network.Gitit2.Helper (
  getConfig,
  getRawContents,
  pathForPage
  ) where

import Control.Applicative ((<$>))
import Control.Exception (handle, throw)
import Data.ByteString.Lazy (ByteString)
import Data.FileStore (FileStoreError(NotFound), retrieve, RevisionId)
import Network.Gitit2.Foundation (config, filestore, GititConfig, GH, HasGitit, page_extension)
import Network.Gitit2.Page (pathForPageP, Page)
import Yesod (getYesod, liftIO)

getConfig :: GH master GititConfig
getConfig = config <$> getYesod

pathForPage :: Page -> GH master FilePath
pathForPage p = do
  conf <- getConfig
  return $ pathForPageP (page_extension conf) p

getRawContents :: HasGitit master
               => FilePath
               -> Maybe RevisionId
               -> GH master (Maybe ByteString)
getRawContents path rev = do
  fs <- filestore <$> getYesod
  liftIO $ handle (\e -> if e == NotFound then return Nothing else throw e)
         $ Just <$> retrieve fs path rev
