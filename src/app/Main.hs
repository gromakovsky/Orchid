import           Orchid.Parser (parseInputFile)

import           Options       (Options (..), getOptions)

main :: IO ()
main = do
    Options {..} <- getOptions
    print =<< parseInputFile optInputFile
