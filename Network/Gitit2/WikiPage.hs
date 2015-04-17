module Network.Gitit2.WikiPage
       (
         extractCategories,
         PageFormat (..),
         WikiPage (..),
         readPageFormat
       )
       where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc (Inline, Block)
import Yesod (Value(String))

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

extractCategories :: M.Map Text Value -> [Text]
extractCategories metadata =
  case M.lookup ("categories" :: Text) metadata of
       Just (String t) -> T.words $ T.replace "," " " t
       _               -> []

-- | The Boolean is True for literate Haskell.
data PageFormat = Markdown Bool
                | RST Bool
                | LaTeX Bool
                | HTML Bool
                | Textile Bool
                | Org Bool
                deriving (Read, Show, Eq )

readPageFormat :: Text -> Maybe PageFormat
readPageFormat s =
  case T.toLower s' of
       "markdown"  -> Just $ Markdown lhs
       "textile"   -> Just $ Textile lhs
       "latex"     -> Just $ LaTeX lhs
       "html"      -> Just $ HTML lhs
       "rst"       -> Just $ RST lhs
       "org"       -> Just $ Org lhs
       _           -> Nothing
 where (s',rest) = T.break (=='+') s
       lhs = rest == "+lhs"

