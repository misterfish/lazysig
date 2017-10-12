module Main where

import Lazysig
    ( parseInput
    , Generate(generate)
    )

main :: IO ()
main = getLine >>= putStrLn . either'' . generate . either' . parseInput where
    either' = either (error "bad parse") id
    either'' = either (error . (++) "bad generate: ") id
