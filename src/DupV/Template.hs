-- | A module for representing Templates, i.e., Strings with
--   varying parts.

module DupV.Template where

import Data.Char
import Data.Map ((!))
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP as ReadP

-- | A template is just a wrapper around a list of TemplateItems,

data Template = Template { items :: [TemplateItem] }

-- | A TemplateItem consists either of a literal string, or a
--   named variable.
   
data TemplateItem 
    = Literal String
    | Variable String

-- | The Read instance of Template.

instance Read Template where
    readsPrec _ = readP_to_S parseTemplate

-- | Load a Template from a file.

loadTemplateFile :: FilePath -> IO Template
loadTemplateFile path = read <$> readFile path

-- | Instantiate a Template from a list of keys and a list of values.

instantiate :: Template -> [String] -> [String] -> String
instantiate template header record =
    let dict = Map.fromList $ zip header record
        fill (Literal s) = s
        fill (Variable v) = dict ! v
    in concat . map fill . items $ template

-- | Parse a Template

parseTemplate :: ReadP Template
parseTemplate = Template <$> many parseTemplateItem where
    parseTemplateItem = parseLiteral <++ parseVariable
    parseVariable = Variable <$>
        (char '{' *> munch1 isAlphaNum <* char '}')
    parseLiteral  = Literal <$> munch1 (`notElem` "{}")
