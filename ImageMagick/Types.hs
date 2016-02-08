module ImageMagick.Types
    (
        ImageFileFormat(..),
        ImageDimensions(..),
        DPI(..),
        FileName,
        ImageFileInformation(..),
        mkImageFileInformation,
        mkDPI
    ) where


import qualified Data.Map.Strict as Map


data ImageFileFormat = TIFF | PNG | JPEG | UNKNOWN
    deriving (Eq)

data ImageDimensions = ImageDimensions { xPixels :: Integer, yPixels :: Integer }
    deriving (Eq, Ord)

data ImageResolution = ImageResolution { xRes :: Integer, yRes :: Integer }
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


instance Show ImageFileFormat where
    show TIFF    = "tiff"
    show PNG     = "png"
    show JPEG    = "jpg"
    show UNKNOWN = "UNKNOWN"


instance Show ImageDimensions where
    show a = show (xPixels a) ++ "x" ++ show (yPixels a)


mkImageFileInformation :: FileName -> ImageFileFormat -> ImageDimensions -> ImageFileInformation
mkImageFileInformation name fmt dims = ImageFileInformation name fmt "" dims


mkDPI :: Integer -> ImageResolution
mkDPI res = DPI res res