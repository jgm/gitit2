module Network.Gitit2.Cache
       where

import           Blaze.ByteString.Builder (toLazyByteString)
import           Control.Monad (filterM, when)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8 as BSU
import           Network.Gitit2.Foundation (GH, cache_dir, use_cache)
import           Network.Gitit2.Helper (getConfig)
import           Network.HTTP (urlDecode)
import           Network.HTTP (urlEncode)
import           System.Directory (createDirectoryIfMissing)
import           System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import           System.FilePath ((</>))
import           System.FilePath (takeDirectory)
import           Yesod (Content(ContentBuilder), liftIO, sendFile, toTypedContent, ToTypedContent, TypedContent(TypedContent))

tryCache :: FilePath -> GH master ()
tryCache path = do
  conf <- getConfig
  when (use_cache conf) $
     do
       let fullpath = cache_dir conf </> path
       exists <- liftIO $ doesDirectoryExist fullpath
       when exists $
          do
            files <- liftIO $ getDirectoryContents fullpath >>=
                               filterM (doesFileExist . (fullpath </>))
            case files of
                 (x:_) -> do
                    let ct = BSU.fromString $ urlDecode x
                    sendFile ct $ fullpath </> x
                 _     -> return ()

caching :: ToTypedContent a
        => FilePath -> GH master a -> GH master a
caching path handler = do
  conf <- getConfig
  if use_cache conf
     then do
       result <- handler
       cacheContent path $ toTypedContent result
       return result
     else handler

cacheContent :: FilePath -> TypedContent -> GH master ()
cacheContent path (TypedContent ct content) = do
  conf <- getConfig
  when (use_cache conf) $
       case content of
            ContentBuilder builder _ -> liftIO $ do
              let fullpath = cache_dir conf </> path </> urlEncode (BSU.toString ct)
              createDirectoryIfMissing True $ takeDirectory fullpath
              B.writeFile fullpath $ toLazyByteString builder
            _ -> liftIO $
              -- TODO replace w logging
              putStrLn $ "Can't cache " ++ path
