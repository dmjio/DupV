module DupV.Template where

import Data.Char
import Data.Map ((!))
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP as ReadP

data Template = Template { items :: [TemplateItem] }
    deriving Show
    
data TemplateItem 
    = Literal String
    | Variable String
    deriving Show
    
instance Read Template where
    readsPrec _ = readP_to_S parseTemplate

loadTemplateFile :: FilePath -> IO Template
loadTemplateFile path = read <$> readFile path

instantiate :: Template -> [String] -> [String] -> String
instantiate template header record =
    let dict = Map.fromList . zip header $ record
        fill (Literal s) = s
        fill (Variable v) = dict ! v
    in concat . map fill . items $ template

parseTemplate :: ReadP Template
parseTemplate = Template <$> ReadP.many parseTemplateItem where
    parseTemplateItem = parseLiteral <++ parseVariable
    parseVariable = Variable <$> (char '{' *> munch1 isAlphaNum <* char '}')
    parseLiteral  = Literal <$> munch1 (`notElem` "{}")
