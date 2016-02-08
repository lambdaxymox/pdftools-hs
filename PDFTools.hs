import Control.Monad
import Data.Either
import System.Environment
import System.IO
import System.Process
import System.Directory (listDirectory)
import Data.Strings (sEndsWith)
import Text.Parsec

import ImageMagick

import qualified Data.Map.Strict as Map


countByDimensions :: [ImageFileInformation] ->  Map.Map ImageDimensions Integer
countByDimensions images = countByDimensions' images Map.empty
    where
        countByDimensions' :: [ImageFileInformation] 
                            -> Map.Map ImageDimensions Integer 
                            -> Map.Map ImageDimensions Integer
        countByDimensions' [] m             = m
        countByDimensions' (image:images) m = countByDimensions' images m'
            where
                m'           = update image m 
                update image = Map.adjust (+1) (imageDimensions image)


groupByDimensions :: [ImageFileInformation] -> Map.Map ImageDimensions [ImageFileInformation]
groupByDimensions images = groupByDimensions' images Map.empty
    where
        groupByDimensions' :: [ImageFileInformation]
                            -> Map.Map ImageDimensions [ImageFileInformation] 
                            -> Map.Map ImageDimensions [ImageFileInformation]
        groupByDimensions' [] m             = m
        groupByDimensions' (image:images) m = groupByDimensions' images m'
            where
                m'           = update image m
                update image = Map.adjust (\l -> image:l) (imageDimensions image)


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


getImageInfo :: (FilePath, [FileName]) -> IO [ImageFileInformation]
getImageInfo (path, files) =
    let
        paths      = map (\f -> path ++ f) files
        pageData   = map identify paths
        parsedData = map (fmap parseImageFileInfo) pageData
    in 
        rights <$> sequence parsedData

images :: FilePath -> IO [ImageFileInformation]
images path = join $ getImageInfo <$> getPagesTiff path


main :: IO ()
main = do
    args <- getArgs
    putStrLn "Got arguments:" 
    print $ show args
