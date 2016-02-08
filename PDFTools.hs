import Control.Monad
import System.Environment
import System.IO
import System.Process
import System.Directory (listDirectory)
import Data.Strings (sEndsWith)
import Text.Parsec

import qualified Data.Map.Strict as Map


data ImageFileExtension = TIFF | PNG | JPEG
    deriving (Eq)

instance Show ImageFileExtension where
    show TIFF = "tiff"
    show PNG  = "png"
    show JPEG = "jpg"

data ImageDimensions = ImageDimensions { xPixels :: Integer, yPixels :: Integer }
    deriving (Eq, Ord, Show)

data DPI = DPI { xDPI :: Integer, yDPI :: Integer }

type FileName = String
type ImageMagickString = String

data ImageFileInformation = ImageFileInformation
    {
        imageFileName       :: FileName,
        imageFileExtensions :: ImageFileExtension,
        imageFilePath       :: FilePath,
        imageDimensions     :: ImageDimensions
    }
    deriving (Show)


mkDPI :: Integer -> DPI
mkDPI res = DPI res res


mkImageInformation :: FileName 
                    -> ImageFileExtension
                    -> FilePath
                    -> ImageDimensions 
                    -> ImageFileInformation
mkImageInformation = ImageFileInformation


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


identifyOpts :: [String] -> FilePath -> IO String
identifyOpts opts path = do
    (_, Just hout, _, _) <- createProcess (proc "identify" (opts ++ [path])) { std_out = CreatePipe }
    s                    <- hGetContents hout
    return s

identifyDefault :: FilePath -> IO String
identifyDefault path = identifyOpts [] path

identifyVerbose :: FilePath -> IO String
identifyVerbose path = identifyOpts ["-verbose"] path

identify = identifyDefault


getPages :: ImageFileExtension -> FilePath -> IO (FilePath, [FilePath])
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


imageMagickParser :: Stream s m Char => ParsecT s u m ([String] -> ImageFileInformation)
imageMagickParser = return $ \ss ->
    let
        fname = ss !! 0
        ext = TIFF
        fpath = ""
        imageDims = ImageDimensions 0 0
    in 
        ImageFileInformation fname ext fpath imageDims


parseDimensions :: Stream s m Char => ParsecT s u m ImageDimensions
parseDimensions = do
    width <- many1 digit
    char 'x'
    height <- try (manyTill digit (oneOf " ")) <|> try (manyTill digit eof)
    return $ ImageDimensions (read width) (read height)




parseImageInfo :: String -> Either ParseError ImageFileInformation
parseImageInfo s = runParser (imageMagickParser <*> splitOnSpace) () "" s


main :: IO ()
main = do
    args <- getArgs
    putStrLn "Got arguments:" 
    print $ show args
