
{-# LANGUAGE LambdaCase #-}

module CharFreq ( computeCharFreq
                , showCharFreq
                ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Char
import Text.Printf
import Data.List
import Data.Function
import qualified Data.Map.Strict as M
import Control.Monad
import qualified Data.ByteString as B

-- Utility module to compute character frequencies
--
-- Example:
--
-- showCharFreq =<< (computeCharFreq <$> Data.Text.IO.readFile "./tweet.txt") 

computeCharFreq :: T.Text -> [(Char, Int)]
computeCharFreq text =
    -- List of character / frequency pairs sorted by descending frequency
      reverse
    . sortBy (compare `on` snd)
    . M.toList
    -- Create a map of character to frequencies
    . foldr (\c m -> M.alter (\case Just n  -> Just $ n + 1 -- Increment frequency
                                    Nothing -> Just 1)      -- New character
                              c m) M.empty
    -- List of characters in text, skip control characters
    . filter (not . isControl)
    . T.unpack
    $ text

showCharFreq :: [(Char, Int)] -> IO ()
showCharFreq cf = do
    putStrLn "Frequency | Chr | Decimal | UTF8 "
    forM_ cf $ \(c, cnt) ->
        printf "%8ix |   %2s | %6i | %s\n"
            cnt
            ([c] :: String)
            (fromEnum c)
            (show . B.unpack . E.encodeUtf8 . T.pack $ [c])

