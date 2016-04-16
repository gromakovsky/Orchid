import           Orchid.Parser (parseInputFile)

main :: IO ()
main = print =<< parseInputFile "test.orc"
