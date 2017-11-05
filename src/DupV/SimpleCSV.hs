-- | A module for parsing newline-terminated CSV.

module DupV.SimpleCSV where

import Data.Functor
import Text.ParserCombinators.ReadP

-- | We represent CSV as as wrapped version of [[String]].

newtype CSV = CSV { content :: [[String]] }

-- | The Read instance just calls our parser.

instance Read CSV where
    readsPrec _ = readP_to_S parseSimpleCSV

-- | Load a CSV file.

loadSimpleCSV :: FilePath -> IO CSV
loadSimpleCSV path = read <$> readFile path

-- | Parse a CSV file.

parseSimpleCSV :: ReadP CSV
parseSimpleCSV = CSV <$> record `endBy` newline <* eof where
    newline = string "\n" <++ string "\r\n"
    record = field `sepBy` char ','
    field = quotedField <++ simpleField
    simpleField = munch (`notElem` ",\n\"")
    quotedField = char '"' *> many quoteChar <* char '"'
    quoteChar = satisfy (/= '"') <++ (string "\"\"" $> '"')
