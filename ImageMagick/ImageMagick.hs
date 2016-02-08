module ImageMagick.ImageMagick
    (
        identify
    )
    where

import System.Process
import System.Environment
import System.IO
import System.Directory (listDirectory)


identifyOpts :: [String] -> FilePath -> IO String
identifyOpts opts path = do
    (_, Just hout, _, _) <- createProcess (proc "identify" (opts ++ [path])) { std_out = CreatePipe }
    s                    <- hGetContents hout
    return s


identifyDefault :: FilePath -> IO String
identifyDefault path = identifyOpts [] path


identifyVerbose :: FilePath -> IO String
identifyVerbose path = identifyOpts ["-verbose"] path


identify :: FilePath -> IO String
identify = identifyDefault

-- TODO: Add convert and mogrify functions
