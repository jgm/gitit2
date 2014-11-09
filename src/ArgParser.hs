module ArgParser
    (readArgs)
where

import Data.Yaml
import Data.Version (showVersion)
import Paths_gitit2 (version, getDataFileName)
import qualified Data.ByteString.Char8 as B
import Error
import System.Console.GetOpt
import System.Environment
import Data.ByteString.UTF8 (fromString)
import qualified Text.Pandoc.UTF8 as UTF8

import Config

readArgs :: String -> IO Conf
readArgs defaultSettingFile = do
  -- parse options to get config file
  args <- getArgs >>= parseArgs

  -- sequence in Either monad gets first Left or all Rights
  opts <- case sequence args of
    Left Help -> err 0 =<< usageMessage
    Left Version -> do
        progname <- getProgName
        err 0 (progname ++ " version " ++
            showVersion version ++ copyrightMessage)
    Left PrintDefaultConfig -> getDataFileName defaultSettingFile >>=
        UTF8.readFile >>= B.putStrLn . fromString >> exitSuccess
    Right xs -> return xs
  settingsFile <- getDataFileName defaultSettingFile
  let settingsFiles =  settingsFile : [f | ConfigFile f <- opts]
  jsonConf <- sequence $ decodeFileOrErr `fmap` reverse settingsFiles
  conf <- parseMonad parseConfig jsonConf
  return $ foldl handleFlag conf opts

decodeFileOrErr :: FromJSON a => String -> IO a
decodeFileOrErr settingsFile = do
    res <- decodeEither `fmap` B.readFile settingsFile
    case res of
             Left e  -> err 3 $ "Error reading configuration file.\n" ++ e
             Right x -> return x


-- taken from gitit v1
data ExitOpt
    = Help
    | Version
    | PrintDefaultConfig

data ConfigOpt
    = ConfigFile FilePath
    | Port Int
    | Debug
    deriving (Eq)

type Opt = Either ExitOpt ConfigOpt

flags :: [OptDescr Opt]
flags =
   [ Option "h" ["help"] (NoArg (Left Help))
        "Print this help message"
   , Option "v" ["version"] (NoArg (Left Version))
        "Print version information"
   , Option "p" ["port"] (ReqArg (Right . Port . read) "PORT")
        "Specify port"
   , Option [] ["print-default-config"] (NoArg (Left PrintDefaultConfig))
        "Print default configuration"
   , Option [] ["debug"] (NoArg (Right Debug))
        "Print debugging information on each request"
   , Option "f" ["config-file"] (ReqArg (Right . ConfigFile) "FILE")
        "Specify configuration file"
   ]

parseArgs :: [String] -> IO [Opt]
parseArgs argv =
  case getOpt Permute flags argv of
    (opts,_,[])  -> return opts
    (_,_,errs)   -> err 1 . (concat errs ++) =<< usageMessage

usageMessage :: IO String
usageMessage = do
  progname <- getProgName
  return $ usageInfo ("Usage:  " ++ progname ++ " [opts...]") flags

copyrightMessage :: String
copyrightMessage = "\nCopyright (C) 2008 John MacFarlane\n" ++
                   "This is free software; see the source for copying conditions.  There is no\n" ++
                   "warranty, not even for merchantability or fitness for a particular purpose."

handleFlag :: Conf -> ConfigOpt -> Conf
handleFlag conf Debug = conf -- TODO
handleFlag conf (Port p) = conf { cfg_port = p }
handleFlag conf _ = conf
