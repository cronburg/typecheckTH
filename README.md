# typecheckTH

This project contains a motivational example for type-checking of compile-time
template haskell expressions. In `src/Grammar.hs` we define a quasiquoter for
a simple grammar language, an example of which can be found in
`test/Grammar.hs`.

## Running

As of the current writing of this, if you run stack test you get the following
spliced template haskell code for a grammar visitor pattern:

```bash
$ stack test
...
[Rule "F" [(["B","c","B"],ParensE (LamE [VarP x,VarP y,VarP z] (AppE (AppE
(ConE Foo) (VarE x)) (VarE z))))],Rule "B" [(["b","b"],ParensE (LamE [VarP
b1,VarP b2] (ConE Bar))),(["F"],VarE id)]]
             
Completed 2 action(s).

$ cat `find . -name *.dump-splices`
test/Grammar.hs:(11,4)-(14,2): Splicing declarations
    Language.Haskell.TH.Quote.quoteDec
      g
      " F := B c B   ~ (\\x y z -> Foo x z)\n\
      \    B := b b     ~ (\\b1 b2 -> Bar)\n\
      \       | F       ~ id\n"
  ======>
    ast_a51X
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
```
