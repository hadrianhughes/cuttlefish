module Main where

import Cuttlefish
import           Options.Applicative
import qualified Data.Text.IO                          as T

data Options = Options { infile :: FilePath }

optionsP :: Parser Options
optionsP = Options <$> strArgument (help "Source file" <> metavar "FILE")

runOpts :: Options -> IO ()
runOpts opts = do
  program <- T.readFile (infile opts)
  let parseTree = runParser programP (infile opts) program
  case parseTree of
    Left  err -> putStrLn $ errorBundlePretty err
    Right ast -> putStrLn $ show ast

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Run the Cuttlefish compiler on the given file."
