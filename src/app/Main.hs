import           Control.Monad (when)

import           Orchid.Lexer  (tokenizeInputFile)
import           Orchid.Parser (parseInputFile)

import           Options       (Options (..), getOptions)

main :: IO ()
main = do
    Options{..} <- getOptions
    when (optDumpTokens) $ print =<< tokenizeInputFile optInputFile
    when (optDumpAST) $ print =<< parseInputFile optInputFile
