module DupV.SimpleCSV where

import Data.Functor
import Text.ParserCombinators.ReadP

loadSimpleCSV :: FilePath -> IO [[String]]
loadSimpleCSV path = parse parseSimpleCSV <$> readFile path

parseSimpleCSV :: ReadP [[String]]
parseSimpleCSV = record `endBy` newline where
    newline = string "\n" <++ string "\r\n"
    record = field `sepBy` char ','
    field = quotedField <++ simpleField
    simpleField = munch (`notElem` ",\n\"")
    quotedField = char '"' *> many quoteChar <* char '"'
    quoteChar = satisfy (/= '"') <++ (string "\"\"" $> '"')
    
parse :: ReadP a -> String -> a
parse parser s = case fst <$> readP_to_S (parser <* eof) s of
    [a] -> a
    []  -> error "no parse"
    _   -> error "ambiguous parse"
