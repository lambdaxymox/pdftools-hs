module ImageTools.Attributes
    (
        countByDimensions,
        groupByDimensions
    )
    where

import ImageMagick.Types

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
                m'             = update image m 
                update image m = case Map.lookup dimensions m of 
                        Nothing -> Map.insert dimensions 1 m
                        Just _  -> Map.adjust (+1) dimensions m
                    where
                        dimensions = imageDimensions image


groupByDimensions :: [ImageFileInformation] -> Map.Map ImageDimensions [ImageFileInformation]
groupByDimensions images = groupByDimensions' images Map.empty
    where
        groupByDimensions' :: [ImageFileInformation]
                            -> Map.Map ImageDimensions [ImageFileInformation] 
                            -> Map.Map ImageDimensions [ImageFileInformation]
        groupByDimensions' [] m             = m
        groupByDimensions' (image:images) m = groupByDimensions' images m'
            where
                m'             = update image m
                update image m = case Map.lookup dimensions m of 
                        Nothing -> Map.insert dimensions [image] m
                        Just _  -> Map.adjust (\l -> image:l) dimensions m
                    where
                        dimensions = imageDimensions image
                        