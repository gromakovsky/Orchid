import           Control.Monad   (when)
import           Data.Maybe      (fromJust)

import           Orchid.Compiler (CompilerExtra (..), CompilerOptions (..),
                                  compile)

import           Options         (Options (..), getOptions)

main :: IO ()
main = do
    Options{..} <- getOptions
    let co =
            CompilerOptions
            { coInputFile = optInputFile
            , coOutputFile = optOutputFile
            , coReturnTokens = optDumpTokens
            , coReturnTree = optDumpAST
            }
    CompilerExtra{..} <- compile co
    when (optDumpTokens) $ putStrLn "Tokens:" >> (print . fromJust $ ceTokens)
    when (optDumpAST) $ putStrLn "AST:" >> (print . fromJust $ ceTree)
