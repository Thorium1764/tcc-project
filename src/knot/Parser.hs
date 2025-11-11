import Lexer
import Control.Monad.State
import qualified Data.Text as T

type LexState = { src :: [Token], ast :: [NodeStmt]}

type Lexer a = Tokenizer LexState a

Data NodeStmt
   = StmtFuncCall T.Text [NodeExpr]
   | StmtVarAssign T.Text NodeExpr
   | StmtVarDeclare T.Text NodeExpr TypeV
   | StmtFuncDeclare T.Text NodeScope TypeV

   | StmtReturn NodeExpr

type NodeScope = [NodeStmt]
type AST = [NodeStmt]
