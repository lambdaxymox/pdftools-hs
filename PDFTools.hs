import Control.Monad
import System.Environment
import System.IO
import System.Process
import System.Directory (listDirectory)
import Data.Strings (sEndsWith)
import Text.Parsec

import ImageMagick

import qualified Data.Map.Strict as Map


data ImageFileFormat = TIFF | PNG | JPEG | UNKNOWN
    deriving (Eq)

instance Show ImageFileFormat where
    show TIFF    = "tiff"
    show PNG     = "png"
    show JPEG    = "jpg"
    show UNKNOWN = "UNKNOWN"

data ImageDimensions = ImageDimensions { xPixels :: Integer, yPixels :: Integer }
    deriving (Eq, Ord, Show)

data DPI = DPI { xDPI :: Integer, yDPI :: Integer }
    deriving (Eq, Ord, Show)

type FileName = String

data ImageFileInformation = ImageFileInformation
    {
        imageFileName       :: FileName,
        imageFileExtensions :: ImageFileFormat,
        imageFilePath       :: FilePath,
        imageDimensions     :: ImageDimensions
    }
    deriving (Show)

mkImageFileInformation :: FileName -> ImageFileFormat -> ImageDimensions -> ImageFileInformation
mkImageFileInformation name fmt dims = ImageFileInformation name fmt "" dims


mkDPI :: Integer -> DPI
mkDPI res = DPI res res


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
                

getPages :: ImageFileFormat -> FilePath -> IO (FilePath, [FilePath])
getPages ext path = getPages' ext' path
    where
        getPages' :: String -> FilePath -> IO (FilePath, [FilePath])
        getPages' ext path = do
            files <- filter (endsWith ext) <$> listDirectory path
            return (path, files)

        ext'     = "." ++ show ext
        endsWith = flip sEndsWith


getImageInfo :: (FilePath, [FilePath]) -> [ImageFileInformation]
getImageInfo (path, files) = []


splitOnSpace :: Stream s m Char => ParsecT s u m [String]
splitOnSpace = sepBy (many (noneOf " ")) spaces


imageMagickParser :: Stream s m Char => ParsecT s u m ImageFileInformation
imageMagickParser = do
    name       <- parseFileName
    spaces
    format     <- parseImageFileFormat
    spaces
    dimensions <- parseDimensions
    return $ mkImageFileInformation name format dimensions


parseFileName :: Stream s m Char => ParsecT s u m FileName
parseFileName = do
    name <- many1 (noneOf ".")
    ext  <- many1 (noneOf " ")
    return $ name ++ ext


parseDimensions :: Stream s m Char => ParsecT s u m ImageDimensions
parseDimensions = do
    width <- many1 digit
    char 'x'
    height <- try (manyTill digit (oneOf " ")) <|> try (manyTill digit eof)
    return $ ImageDimensions (read width) (read height)


parseImageFileFormat :: Stream s m Char => ParsecT s u m ImageFileFormat
parseImageFileFormat =  (try (spaces >> string "TIFF") >> return TIFF)
                    <|> (try (spaces >> string "TIF")  >> return TIFF)
                    <|> (try (spaces >> string "JPG")  >> return JPEG)
                    <|> (try (spaces >> string "JPEG") >> return JPEG)
                    <|> (try (spaces >> string "PNG")  >> return PNG)
                    <|> (spaces      >> skipMany upper >> return UNKNOWN)

parseImageFileInfo :: String -> Either ParseError ImageFileInformation
parseImageFileInfo s = runParser imageMagickParser () "" s


main :: IO ()
main = do
    args <- getArgs
    putStrLn "Got arguments:" 
    print $ show args
