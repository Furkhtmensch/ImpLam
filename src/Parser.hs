module Parser where

import Definer
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Data.Map as Map
import qualified Text.Parsec.Token as Token


-- this could become more expansive
-- therefore, I should preemptively
-- clear the way for easier changes

languageDef :: GenLanguageDef String st Identity
languageDef =
  emptyDef
    { Token.commentStart = []
    , Token.commentEnd = []
    , Token.commentLine = "--"
    , Token.identStart = oneOf myLower
    , Token.identLetter = oneOf myLower <|> char '\''
    , Token.opStart = oneOf "\\λ."
    , Token.opLetter = oneOf ""
    , Token.reservedNames = []
    , Token.reservedOpNames =
      [ "\\"
      , "λ"
      , "."
      ]
    , Token.caseSensitive = True
    }

myLower :: String
myLower = "abcdefghijklmnopqrstuvwxyz"

lexer :: Token.GenTokenParser String st Identity
lexer = Token.makeTokenParser languageDef

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser Term -> Parser Term
parens = Token.parens lexer

parseTerm :: NameToIndex -> Parser Term
parseTerm nTI@nameToIndex = do
  spaces
  term <- (parseApplication nTI) <|> (parseSingleton nTI)
  spaces
  return term

parseSingleton :: NameToIndex -> Parser Term
parseSingleton nTI@nameToIndex = do
  parseVariable nTI <|> parseAbstraction nTI <|> parens (parseApplication nTI)

parseSimpleVariable :: Parser String
parseSimpleVariable = do
  singleLetter <- oneOf myLower
  apostrophes <- many $ char '\''
  return (singleLetter:apostrophes)
  
-- if index is -1, it's a free variable
-- if it's 0, it's the binding of a variable
-- otherwise, it's a bound variable
parseVariable :: NameToIndex -> Parser Term
parseVariable nTI@nameToIndex = do
  spaces
  name <- parseSimpleVariable
  spaces
  let index = Map.findWithDefault (-2) name nTI + 1
  return (Variable name index)


parseAbstraction :: NameToIndex -> Parser Term
parseAbstraction nTI@nameToIndex = do
  reservedOp "\\" <|> reservedOp "λ"
  boundVariables <- sepBy1 parseSimpleVariable spaces
  reservedOp "."
  let nTI' = updateNameToIndex boundVariables nTI
  abstractionBody <- parseTerm nTI'
  return $ uncurrify boundVariables abstractionBody
  where
    updateNameToIndex boundVariables nTI@nameToIndex =
      let lenBV = length boundVariables
       in foldl (\x (y, z) -> Map.insert y z x) nTI
            $ zip boundVariables [lenBV - 1, lenBV - 2..0]
    uncurrify boundVariables abstractionBody =
      foldr Abstraction abstractionBody
        $ map (\(x, y) -> Variable x y)
        $ zip boundVariables
        $ repeat 0

parseApplication :: NameToIndex -> Parser Term
parseApplication nTI@nameToIndex = do
  subterms <- sepBy1 (parseSingleton nTI) spaces
  return $ foldl1 Application subterms
  
