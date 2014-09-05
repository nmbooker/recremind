module Atd.IO (
    scheduleAt
) where

import System.Process (readProcessWithExitCode)
import Atd.Calc (Script)
import System.Exit (ExitCode(..))

scheduleAt ::  [String]  -- ^ list of arguments for at
            -> Script    -- ^ The script to run
            -> IO (Either (Int, String) String) -- ^ Output, whether failure or success
scheduleAt args script = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode
                                    "at" args script
    return $ case exitCode of
        ExitFailure code -> Left (code, stderr ++ stdout)
        ExitSuccess -> Right $ stderr ++ stdout
