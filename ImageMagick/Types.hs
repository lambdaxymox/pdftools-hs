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


instance Show ImageFileFormat where
    show TIFF    = "tiff"
    show PNG     = "png"
    show JPEG    = "jpg"
    show UNKNOWN = "UNKNOWN"


mkImageFileInformation :: FileName -> ImageFileFormat -> ImageDimensions -> ImageFileInformation
mkImageFileInformation name fmt dims = ImageFileInformation name fmt "" dims


mkDPI :: Integer -> DPI
mkDPI res = DPI res res