{-# LANGUAGE QuasiQuotes, TemplateHaskell, DeriveLift #-}
module Grammar
  ( g
  , Rule(..)
  ) where
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH
import Text.Parsec.String (Parser(..))
import Text.Parsec
import qualified Text.Parsec.Token as PT
import qualified Language.Haskell.Meta as LHM
import qualified Text.Parsec.Prim as PP
import Text.Parsec.Language (haskellStyle)
import Language.Haskell.TH.Syntax (lift, Lift(..))

-- | A 'QuasiQuoter' for parsing a simple *grammar* language and splicing in
-- data type declarations for each grammar nonterminal symbol as well as
-- functions 
g :: QuasiQuoter
g =  QuasiQuoter
  (error "parse exp")     -- quasiquoter that runs when expecting a Haskell expression
  (error "parse pattern") -- quasiquoter that runs when expecting a Haskell pattern
  (error "parse type")    -- quasiquoter that runs when expecting a Haskell type
  dparse

-- | Template Haskell declaration parser for the 'g' quasiquoter.
dparse :: String -> Q [Dec]
dparse input = codegen $
  case PP.parse (many1 $ whitespace >> prodRule) "" input of
    Left err    -> error $ show err
    Right rules -> rules

codegen :: [Rule] -> Q [Dec]
codegen rules = [d| ast = $(lift rules) |]

prodRule :: Parser Rule
prodRule = do
  nt <- lhs
  reservedOp ":="
  as <- alphas
  reservedOp "~"
  exp <- haskellExp
  return $ Rule nt as exp

-- | Left hand side of a production rule
lhs :: Parser NT
lhs = identifier

-- | Right hand side of a production rule
alphas :: Parser [Alpha]
alphas = many1 identifier

-- | A parser for Haskell expressions inside of the quasiquoter
haskellExp :: Parser TH.Exp
haskellExp = do
  str <- manyTill anyChar (char '\n')
  whitespace
  case LHM.parseExp str of
    Left err  -> error err
    Right exp -> return exp

-- | Lexical analyzer for our eDSL
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser $ haskellStyle
  { PT.reservedOpNames = [":=", "~"] }

-- | Parser for reserved operators in our eDSL
reservedOp = PT.reservedOp lexer
identifier = PT.identifier lexer
whitespace = PT.whiteSpace lexer

data Rule   = Rule NT [Alpha] TH.Exp
  deriving (Lift)

type NT     = String
type Term   = String
type Alpha  = String

