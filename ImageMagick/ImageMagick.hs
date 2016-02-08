module ImageMagick.ImageMagick
    (
        identify
    )
    where

import System.Process
import System.Environment
import System.Directory (listDirectory)

import qualified System.IO as IO


hGetContents :: IO.Handle -> IO.IO String
hGetContents h = IO.hGetContents h >>= \s -> length s `seq` return s


identifyOpts :: [String] -> FilePath -> IO String
identifyOpts opts path = do
    (_, Just hout, _, _) <- createProcess (proc "identify" (opts ++ [path])) { std_out = CreatePipe }
    s                    <- hGetContents hout
    IO.hClose hout
    return s


identifyDefault :: FilePath -> IO String
identifyDefault path = identifyOpts [] path


identifyVerbose :: FilePath -> IO String
identifyVerbose path = identifyOpts ["-verbose"] path


identify :: FilePath -> IO String
identify = identifyDefault

-- TODO: Add convert and mogrify functions
