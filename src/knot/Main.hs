module Main where
import Lexer
import Control.Monad.State
import qualified Data.Text as T

main :: IO ()
main = do
   let initState = TokState (T.pack "char what <<= \'\\u2588\'") 0 []
       finState = execState tokenize initState
   print (reverse $ tokens finState)

