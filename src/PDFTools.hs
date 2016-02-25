import Control.Monad (join)
import Data.Either   (rights)
import Data.Strings  (sEndsWith)

import System.Environment
import System.IO
import System.Process
import System.Directory (listDirectory)

import ImageMagick
import ImageTools


getPages :: ImageFileFormat -> FilePath -> IO (FilePath, [FileName])
getPages ext path = getPages' ext' path
    where
        getPages' :: String -> FilePath -> IO (FilePath, [FileName])
        getPages' ext path = do
            files <- filter (endsWith ext) <$> listDirectory path
            return (path, files)

        ext'     = "." ++ show ext
        endsWith = flip sEndsWith


getPagesTiff :: FilePath -> IO (FilePath, [FileName])
getPagesTiff = getPages TIFF

getPagesPng :: FilePath -> IO (FilePath, [FileName])
getPagesPng = getPages PNG


getPageInfo :: (FilePath, [FileName]) -> IO [ImageFileInformation]
getPageInfo (path, files) =
    let
        paths      = map (\f -> path ++ f) files
        pageData   = map identify paths
        parsedData = map (fmap parseImageFileInfo) pageData
    in 
        rights <$> sequence parsedData

images :: FilePath -> IO [ImageFileInformation]
images path = join $ getPageInfo <$> getPagesTiff path


main :: IO ()
main = do
    args <- getArgs
    putStrLn "Got arguments:" 
    print $ show args
