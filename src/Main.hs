module Main where

import DupV.SimpleCSV
import DupV.Template
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [variationFile,templateFile] -> do
            template <- loadTemplateFile templateFile
            header:variations <- loadSimpleCSV variationFile
            putStr . concat . (`map` variations) $ \variation ->
                instantiate template header variation
        _ -> do
            hPutStrLn stderr "Usage: spam variationFile templateFile"
            exitFailure
