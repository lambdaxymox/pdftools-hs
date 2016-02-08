module ImageMagick.DefaultParser
    (
        imageMagickParser,
        parseImageFileInfo
    ) where

import Text.Parsec
import ImageMagick.Types


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
