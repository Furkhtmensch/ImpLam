module Main where

import Definer
import Parser
import Reducer
import Printer
import Text.Parsec
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "Greetings! This program will receive 2 lambda terms each time."
  putStrLn "You may choose how many reductions you want made for each term."
  putStrLn "Also, you may choose to see the process of resolution!"
  putStrLn ""
  putStrLn "Would you like to see all reductions, or only the final one?"
  putStrLn "\"Yes\" for all, \"No\" for final one only, or gibberish for all: "
  showAllRedexes <- getLine
  putStrLn "Now, insert the first term here: "
  txt <- getLine
  let parseResult = runParser (parseTerm Map.empty) () "" txt
      result =
        case parseResult of
          Left e  -> Variable (show e) 0
          Right p -> p
      freeVars = Set.toList $ getFreeVariableSet result
      boundVars = Set.toList $ getBoundVariableSet result
      allVars = Set.toList $ getAllVariableSet result
  putStrLn "How many reductions would you like for term1? (insert a negative number for as many as possible): "
  redexes1 <- getLine
  putStrLn ""
  putStrLn "Info regarding the first term (freeVars, boundVars, allVars, term1 itself, respectively): "
  putStrLn $ show freeVars
  putStrLn ""
  putStrLn $ show boundVars
  putStrLn ""
  putStrLn $ show allVars
  putStrLn ""  
  putStrLn $ unpackText $ showTermCurried result
  putStrLn ""
  putStrLn "Now, insert the second term here: "
  putStrLn ""
  txt2 <- getLine
  let parseResult2 = runParser (parseTerm Map.empty) () "" txt2
      result2 =
        case parseResult2 of
          Left e  -> Variable (show e) 0
          Right p -> p
      freeVars2 = Set.toList $ getFreeVariableSet result2
      boundVars2 = Set.toList $ getBoundVariableSet result2
      allVars2 = Set.toList $ getAllVariableSet result2
  putStrLn "How many reductions would you like for term2? (insert a negative number for as many as possible): "
  redexes2 <- getLine
  putStrLn ""
  putStrLn "Info regarding the second term: (freeVars, boundVars, allVars, term2 itself, respectively)"
  putStrLn $ show freeVars
  putStrLn ""
  putStrLn $ show boundVars
  putStrLn ""
  putStrLn $ show allVars
  putStrLn ""  
  putStrLn $ unpackText $ showTermCurried result2
  putStrLn ""
  if (showAllRedexes /= "No")
    then do
      putStrLn "Here are the reductions for the first term: "
      reduceManyIO result $ read redexes1
      putStrLn "Here are the reductions for the second term: "
      reduceManyIO result2 $ read redexes2
    else return ()
  let firstRedex  = reduceMultiple result $ read redexes1
      secondRedex = reduceMultiple result2 $ read redexes2
  putStrLn ""
  putStrLn "Are both the initial term1 and its reduced form Alpha Equivalent? Check here: "
  putStrLn $ show $ isAlphaEquivalent firstRedex result
  putStrLn ""
  putStrLn "Are both the initial term2 and its reduced form Alpha Equivalent? Check here: "
  putStrLn $ show $ isAlphaEquivalent secondRedex result2
  putStrLn ""  
  putStrLn "Are both terms Alpha Equivalent? Check here: "
  putStrLn $ show $ isAlphaEquivalent firstRedex secondRedex
  putStrLn ""
  putStrLn "Check first term reduced as requested here: "
  putStrLn $ unpackText $ showTermCurried firstRedex
  putStrLn ""
  putStrLn "Check second term reduced as requested here: "
  putStrLn $ unpackText $ showTermCurried secondRedex
  putStrLn ""
  putStrLn "Next iteration of the program has started: "
  putStrLn ""
  main


reduceManyIO :: Term -> Int -> IO ()
reduceManyIO term 0 = return ()
reduceManyIO term n = do
  let (termRedex, reduced) = betaReduction term NoReduction
  putStrLn ""
  putStrLn $ unpackText $ showTermCurried termRedex
  if reduced == NoReduction
    then return ()
  else if n < 0
    then reduceManyIO termRedex n
    else reduceManyIO termRedex (n - 1)
