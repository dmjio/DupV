-- | dupv -- a program for duplicating text with variations.

module Main where

import DupV.SimpleCSV
import DupV.Template
import System.Environment
import System.Exit
import System.IO

-- | The main act

main :: IO ()
main = do
    args <- getArgs
    case args of
        [variationFile,templateFile] -> do
            template <- loadTemplateFile templateFile
            header:variations <- content <$> loadSimpleCSV variationFile
            putStr . concat . (`map` variations) $ \variation ->
                instantiate template header variation
        _ -> do
            hPutStrLn stderr "Usage: spam variationFile templateFile"
            exitFailure
