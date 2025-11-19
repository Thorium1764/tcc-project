import Lexer
import Control.Monad.State
import qualified Data.Text as T

data LexState = LexState { src :: [Token], ast :: Either String AST, index :: Int}

type Lexer a = State LexState a

data NodeStmt
   = StmtFuncCall T.Text [NodeExpr]
   | StmtVarAssign T.Text NodeExpr
   | StmtVarDeclare T.Text NodeExpr TypeV
   | StmtFuncDeclare T.Text NodeScope TypeV
   | StmtIfElse NodeExpr NodeStmt NodeStmt
   | StmtExpr NodeExpr
   | StmtReturn NodeExpr

data NodeExpr
   = TermExpr NodeTerm
   | BinExpr BinOp NodeExpr NodeExpr
   | ExprStmt NodeStmt

data NodeTerm
   = ParenTerm NodeExpr
   | UnOpTerm UnOp NodeExpr
   | Literal TypeV Token
   | Identifier Value

data TypeV
   = IntT
   | CharT

data BinOp = Add | Sub | Mult | Div | Mod | LShiftOp | RShiftOp

data UnOp = NotOp | BNotOp | Sizeof

type NodeScope = [NodeStmt]
type AST = [NodeStmt]


parse :: Lexer ()
parse = do
   tok <- peek 0
   if tok == (EmptyToken)
      then modify (\st -> st {ast = Left "Error: " ++ show $ value $ tok ++ " is not a valid float.\n", index = index st + 1})
      else 
   where
      peek :: Int -> Lexer ()
      peek offset = 
         if (

