{-# LANGUAGE TemplateHaskell #-}

import           Data.FileEmbed  (embedStringFile, makeRelativeToProject)
import           Data.String     (IsString)
import qualified Data.Text.IO    as TIO
import           Serokell.Util   (measureTime_, show')

import           Orchid.Executor (Optimization (..), executeProgram)

tail_recSource
    :: IsString s
    => s
tail_recSource =
    $(makeRelativeToProject "bench/tail_rec.orc" >>= embedStringFile)

run :: [Optimization] -> IO ()
run opts =
    TIO.putStrLn . show' =<<
    measureTime_ (executeProgram opts "tail_rec.orc" tail_recSource mempty)

main :: IO ()
main = do
    putStrLn "Running without tail recursion optimization…"
    run []
    putStrLn "Running with tail recursion optimization…"
    run [TailRecursionOptimization]
