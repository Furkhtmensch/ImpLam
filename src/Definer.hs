module Definer where

import qualified Data.Set as Set
import qualified Data.Map as Map

type Name = String
type Index = Int

data Term
  = Variable Name Index
  | Abstraction Term Term
  | Application Term Term
  deriving (Show, Eq)

type NameToIndex = Map.Map String Int

type BoundVariables = [Name]
type AbstractionBody = Term

type VariableSet = Set.Set Name
type LambdaState = Term

data Reduction
  = Reduction Index Term
  | EtaReduced
  | NoReduction
  deriving (Show, Eq)

type NameToNewName = Map.Map String String

type VariableSet4 = (VariableSet, VariableSet, VariableSet, VariableSet)

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

snd4 :: (a, b, c, d) -> b
snd4 (_, y, _, _) = y

trd4 :: (a, b, c, d) -> c
trd4 (_, _, z, _) = z

frt4 :: (a, b, c, d) -> d
frt4 (_, _, _, w) = w
