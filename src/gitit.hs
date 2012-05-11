{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, FlexibleInstances,
             ScopedTypeVariables #-}
import Network.Gitit
import Yesod
import Yesod.Static
import Data.FileStore

data Master = Master { getGitit :: Gitit }
mkYesod "Master" [parseRoutes|
/ SubsiteR Gitit getGitit
|]

instance Yesod Master

instance RenderMessage Master FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage Master GititMessage where
    renderMessage x = renderMessage (getGitit x)

instance YesodGitit Master where
  maybeUser = return $ Just $ GititUser "Dummy" "dumb@dumber.org"
  requireUser = return $ GititUser "Dummy" "dumb@dumber.org"

main :: IO ()
main = do
  let conf = GititConfig{ wiki_path = "wikidata" }
  let fs = gitFileStore $ wiki_path conf
  st <- staticDevel "static"
  warpDebug 3000 $ Master (Gitit{ config    = conf
                                , filestore = fs
                                , getStatic = st
                                })



