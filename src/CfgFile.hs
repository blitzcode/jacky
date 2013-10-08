
{-# LANGUAGE OverloadedStrings #-}

module CfgFile (CfgMap, loadCfgFile) where

import Data.Attoparsec.Char8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as M
import Control.Applicative
import Control.Monad (void)

-- Parse simple configuration file format:
-- 
-- # Comment
-- key = value

type CfgMap = M.Map String String

cfgParser :: Parser CfgMap
cfgParser = M.fromList <$> (many $ cfgLineParser <* endOfLine)
    where cfgLineParser :: Parser (String, String)
          cfgLineParser   = do skipCmtAndEmpty
                               key <- takeTill (\c -> c == ' ' || c == '=')
                               void . skipWS $ char '='
                               value <- takeTill (== '\n')
                               return (B8.unpack key, B8.unpack value)
          skipWS p        = skipSpace *> p <* skipSpace
          skipCmtAndEmpty = void . try . many
                                 $  char '#' *> takeTill (== '\n') *> char '\n' -- Comment
                                <|> char '\n'                                   -- Empty line

loadCfgFile :: FilePath -> IO CfgMap
loadCfgFile fn = do
    contents <- B.readFile fn
    case parseOnly cfgParser contents of
        Left  err -> error err
        Right m   -> return m

