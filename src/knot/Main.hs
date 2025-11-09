module Main where
import Lexer
import Control.Monad.State

main :: IO ()
main = do
   let initState = TokState "char what <<= \'\\u2588\'" 0 []
       finState = execState tokenize initState
   print (reverse $ tokens finState)

