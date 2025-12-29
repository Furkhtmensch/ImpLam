module Printer where

import Definer
import Parser
import Reducer
import qualified Data.Monoid as T
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T

unpackText :: T.Builder -> String
unpackText s = T.unpack $ T.toLazyText s

symbolBuild :: String -> T.Builder
symbolBuild "λ" = T.fromString "λ"
symbolBuild "(" = T.fromString "("
symbolBuild ")" = T.fromString ")"
symbolBuild "." = T.fromString "."
symbolBuild " " = T.fromString " "

showTerm :: Term -> T.Builder
showTerm (Variable name index) = T.fromString name
showTerm (Abstraction term1 term2) =
  symbolBuild "("
    `T.mappend` symbolBuild "λ"
    `T.mappend` showTerm term1
    `T.mappend` symbolBuild "."
    `T.mappend` showTerm term2
    `T.mappend` symbolBuild ")"
showTerm (Application term1@(Abstraction _ _) term2) =
  symbolBuild "("
    `T.mappend` showTerm term1
    `T.mappend` symbolBuild ")"
    `T.mappend` symbolBuild " "
    `T.mappend` showTerm term2
showTerm (Application term1 term2) =
  symbolBuild "("
    `T.mappend` showTerm term1
    `T.mappend` symbolBuild " "
    `T.mappend` showTerm term2
    `T.mappend` symbolBuild ")"

showTermCurried :: Term -> T.Builder
showTermCurried (Variable name index) = T.fromString name
showTermCurried term@(Abstraction term1 term2) =
  symbolBuild "λ" `T.mappend` getCurrying term
showTermCurried (Application term1 term2@(Application _ _)) =
  showTermCurried term1 `T.mappend` parensCurry term2
showTermCurried term@(Application (Application _ _) _) = getCurrying term
showTermCurried (Application term1 term2) =
  parensCurry term1 `T.mappend` parensCurry term2

parensCurry :: Term -> T.Builder
parensCurry term =
  case term of
    Abstraction _ _ ->
      symbolBuild "("
        `T.mappend` showTermCurried term
        `T.mappend` symbolBuild ")"
    _ -> showTermCurried term

getCurrying :: Term -> T.Builder
getCurrying (Abstraction (Variable name _) term2@(Abstraction _ _)) =
  T.fromString name `T.mappend` getCurrying term2
getCurrying (Abstraction (Variable name _) term2) =
  T.fromString name
    `T.mappend` symbolBuild "."
    `T.mappend` showTermCurried term2
getCurrying (Application term1@(Application _ _) term2) =
  getCurrying term1 `T.mappend` parensCurry term2
getCurrying term@(Application _ _) = showTermCurried term
