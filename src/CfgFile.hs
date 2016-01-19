
{-# LANGUAGE OverloadedStrings #-}

module CfgFile (CfgMap, loadCfgFile) where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TI
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
                               return (T.unpack key, T.unpack value)
          skipWS p        = skipSpace *> p <* skipSpace
          skipCmtAndEmpty = void . try . many
                                 $  char '#' *> takeTill (== '\n') *> char '\n' -- Comment
                                <|> char '\n'                                   -- Empty line

loadCfgFile :: FilePath -> IO CfgMap
loadCfgFile fn = do
    contents <- TI.readFile fn
    case parseOnly cfgParser contents of
        Left  err -> error err
        Right m   -> return m

