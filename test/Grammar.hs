{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Grammar
import Language.Haskell.TH.Syntax

[g| F := B c B   ~ (\x y z -> Foo x z)
    B := b b     ~ (\b1 b2 -> Bar)
    B := F       ~ id
|]

main :: IO ()
main = putStrLn "unimplemented"

