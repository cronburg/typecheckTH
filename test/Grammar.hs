{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where
import Grammar
import Language.Haskell.TH.Syntax

data Foo =
    Foo Foo Foo
  | Bar
  deriving (Eq, Show)

[g| F := B c B   ~ (\x y z -> Foo x z)
    B := b b     ~ (\b1 b2 -> Bar)
       | F       ~ id
|]

main :: IO ()
main = print ast

{-
test/Grammar.hs:(11,4)-(14,2): Splicing declarations
    Language.Haskell.TH.Quote.quoteDec
      g
      " F := B c B   ~ (\\x y z -> Foo x z)\n\
      \    B := b b     ~ (\\b1 b2 -> Bar)\n\
      \       | F       ~ id\n"
  ======>
    ast_a51O
      = [Rule
           "F"
           [(["B", "c", "B"], 
             ParensE
               (LamE
                  [VarP (Name (mkOccName "x") NameS),
                   VarP (Name (mkOccName "y") NameS),
                   VarP (Name (mkOccName "z") NameS)]
                  (AppE
                     (AppE
                        (ConE (Name (mkOccName "Foo") NameS))
                        (VarE (Name (mkOccName "x") NameS)))
                     (VarE (Name (mkOccName "z") NameS)))))],
         Rule
           "B"
           [(["b", "b"], 
             ParensE
               (LamE
                  [VarP (Name (mkOccName "b1") NameS),
                   VarP (Name (mkOccName "b2") NameS)]
                  (ConE (Name (mkOccName "Bar") NameS)))),
            (["F"], VarE (Name (mkOccName "id") NameS))]]
    visit_F [p_1, p_2, p_3] = (\ x y z -> Foo x z) p_1 p_2 p_3
    visit_B [p_1, p_2] = (\ b1 b2 -> Bar) p_1 p_2
    visit_B [p_1] = id p_1
-}

