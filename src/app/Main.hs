import           Orchid.Lexer  (tokenizeInputFile)
import           Orchid.Parser (parseInputFile)

import           Options       (Options (..), getOptions)

main :: IO ()
main = do
    Options {..} <- getOptions
    print =<< tokenizeInputFile optInputFile
    print =<< parseInputFile optInputFile
