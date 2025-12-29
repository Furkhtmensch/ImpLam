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

showTermSyntaxSugar :: Term -> T.Builder
showTermSyntaxSugar (Variable name index) = T.fromString name
showTermSyntaxSugar term@(Abstraction term1 term2) =
  symbolBuild "λ" `T.mappend` getSyntaxSugar term
showTermSyntaxSugar (Application term1 term2@(Application _ _)) =
  showTermSyntaxSugar term1 `T.mappend` parensSyntaxSugar term2
showTermSyntaxSugar term@(Application (Application _ _) _) = getSyntaxSugar term
showTermSyntaxSugar (Application term1 term2) =
  parensSyntaxSugar term1 `T.mappend` parensSyntaxSugar term2

parensSyntaxSugar :: Term -> T.Builder
parensSyntaxSugar term =
  case term of
    Abstraction _ _ ->
      symbolBuild "("
        `T.mappend` showTermSyntaxSugar term
        `T.mappend` symbolBuild ")"
    _ -> showTermSyntaxSugar term

getSyntaxSugar :: Term -> T.Builder
getSyntaxSugar (Abstraction (Variable name _) term2@(Abstraction _ _)) =
  T.fromString name `T.mappend` getSyntaxSugar term2
getSyntaxSugar (Abstraction (Variable name _) term2) =
  T.fromString name
    `T.mappend` symbolBuild "."
    `T.mappend` showTermSyntaxSugar term2
getSyntaxSugar (Application term1@(Application _ _) term2) =
  getSyntaxSugar term1 `T.mappend` parensSyntaxSugar term2
getSyntaxSugar term@(Application _ _) = showTermSyntaxSugar term
