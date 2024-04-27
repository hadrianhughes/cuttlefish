module Main where

import           Cuttlefish
import           Options.Applicative
import           Text.Pretty.Simple

data Action = Ast | Sast
data Options = Options { action :: Action, infile :: FilePath }

actionP :: Parser Action
actionP = flag' Ast  (long "ast"  <> short 'a' <> help "Print the AST")
      <|> flag' Sast (long "sast" <> short 's' <> help "Print the SAST")

optionsP :: Parser Options
optionsP = Options
  <$> actionP
  <*> strArgument (help "Source file" <> metavar "FILE")

runOpts :: Options -> IO ()
runOpts (Options action infile) = do
  program <- readFile infile
  let parseTree = runParser programP program
  case parseTree of
    Left  err -> putStrLn $ show err
    Right ast ->
      case action of
        Ast -> pPrint ast
        _   -> case checkProgram ast of
                 Left  err' -> pPrint err'
                 Right sast -> pPrint sast

main :: IO ()
main = runOpts =<< execParser (optionsP `withInfo` infoString)
  where
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    infoString = "Run the Cuttlefish compiler on the given file."
