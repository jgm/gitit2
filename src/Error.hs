module Error
    (warn, err, exitSuccess)
where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))


warn :: String -> IO ()
warn = hPutStrLn stderr

err :: Int -> String -> IO a
err code msg = do
  hPutStrLn stderr msg
  exitWith $ ExitFailure code
  return undefined
