module Types.Snowflake where

import Data.Array
import Data.Ix
import Data.Bits

----------------------------------------------------------

newtype Snowflake = Snowflake
    { bitArray :: Array Int Bool
    }

----------------------------------------------------------

instance Show Snowflake where
    show snowflake = map morph $ elems (bitArray snowflake)
        where
            morph :: Bool -> Char
            morph True  = '1'
            morph False = '0'

----------------------------------------------------------

-- | The 'createSnowflake' function takes a String of 64 bits and returns the corresponding Snowflake
createSnowflake :: String -> Maybe Snowflake
createSnowflake str
    | length str == 64 = Just $ Snowflake $ array (1, 64) $ zip [1..64] (strAsBoolList str)
    | otherwise        = Nothing
    where
        strAsBoolList :: String -> [Bool]
        strAsBoolList s = map morph s

        morph :: Char -> Bool
        morph '1' = True
        morph '0' = False
        morph _   = undefined

timestamp :: Snowflake -> Int
timestamp snow = (bits snow `shiftR` 22) + 1420070400000
        
internalWorkerId :: Snowflake -> Int
internalWorkerId snow = (bits snow .&. 0x3E0000) `shiftR` 27

internalProcessId :: Snowflake -> Int
internalProcessId snow = (bits snow .&. 0x1F000) `shiftR` 12

increment :: Snowflake -> Int
increment snow = bits snow .&. 0xFFF

----------------------------------------------------------

bits :: Snowflake -> Int
bits :: Snowflake -> Int
bits snowflake = bitArrayToInt $ bitArray snowflake

bitArrayToInt :: Array Int Bool -> Int
bitArrayToInt array = foldl (\acc current -> acc * 2 + currentAsInt current) 0 $ elems array
    where
        currentAsInt :: Bool -> Int
        currentAsInt True  = 1
        currentAsInt False = 0