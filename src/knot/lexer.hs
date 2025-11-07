module Lexer where

import Data.Char

data TokenType
   = Identifier
   | Mutable
   | Inline
   | Define
   | Include
   | Function
   | Unsigned
   | U8
   | I8
   | U16
   | I16
   | U32
   | I32
   | U64
   | I64
   | Var
   | IntT
   | CharT
   | FloatT
   | DoubleT
   | Void
   | Struct
   | Enum
   | Union
   | IfT
   | Else
   | Match
   | Is
   | Typedef
   | Sizeof
   | Semicolon
   | Equal
   | SingleArrow
   | DoubleArrow
   | Equals
   | Nequals
   | Lequals
   | Grequals
   | Lesser
   | Greater
   | Pipe
   | BXOr
   | Ampersand
   | And
   | Or
   | Dollar
   | Not
   | BNot
   | Plus
   | Minus
   | Star
   | FSlash
   | BSlash
   | Modulo
   | IntLit
   | StrLit
   | FloatLit --also used with e notation
   | HexLit
   | BinLit
   | OctLit
   | CharLit
   | Colon
   | Underscore
   | Comma
   | Dot -- also used for function composition
   | OpenBracket
   | CloseBracket
   | OpenSquare
   | CloseSquare
   | OpenCurly
   | CloseCurly
   | Extern
   | Increment
   | Decrement
   | AddAssign
   | SubAssign
   | MultAssign
   | DivAssign
   | AndAssign
   | OrAssign
   | ModAssign
   | XOrAssign
   | RShiftAssign
   | LShifAssign
   | RShift
   | LShift
   | QMark
   | Return
   | For
   deriving (Show, Eq)

data Value
   = FLoatV Float
   | IntV Int
   | CharV Char
   | StringV String
   | None
   deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, value :: Value} | EmptyToken deriving (Show, Eq)

tokenize :: String -> Int -> [Token] -> [Token]
tokenize src index tokens
   | (peek index 0) == '0' && (peek index 1) == 'x'
   = let (buf, index2) = completeHex (index + 2) ""
         buf2 = '0' : 'x' : buf
         new_tokens = (Token HexLit (IntV (read buf2))) : tokens
     in tokenize src index2 new_tokens

   | (peek index 0) == '0' && (peek index 1) == 'b'
   = let (buf, index2) = completeBin (index + 2) ""
         buf2 = '0' : 'b' : buf
         new_tokens = (Token BinLit (IntV (read buf2))) : tokens
     in tokenize src index2 new_tokens

   | (peek index 0) == '0' && (peek index 1) == 'o'
   = let (buf, index2) = completeOct (index + 2) ""
         buf2 = '0' : 'o' : buf
         new_tokens = (Token OctLit (IntV (read buf2))) : tokens
     in tokenize src index2 new_tokens

   | (peek index 0) == '"'
   = let (tok, index2) = readStr (index + 1) ""
         new_tok = tok : tokens
     in tokenize src index2 new_tok

   | (peek index 0) == '\''
   = let (tok, index2) = readChar (index + 1) ""
         new_tok = tok : tokens
     in tokenize src index2 new_tok

   | isDigit (peek index 0)
   = let (tok, index2) = completeNum index ""
         new_tok = tok : tokens
     in tokenize src index2 new_tok

   | isAlpha (peek index 0)
   = let (buf, index2) = completeWord index ""
         new_tok = (identifyKeywords buf) : tokens
     in tokenize src index2 new_tok

   | (peek index 0) == '/' && (peek index 1) == '/'
   = let index2 = skipComm index
     in tokenize src index2 tokens

   | (peek index 0) == '/' && (peek index 1) == '*'
   = let index2 = skipMultComm index
     in tokenize src index2 tokens

   | (charExpr index) /= EmptyToken
   = let (tok, index2) = charExpr index
     in tokenize src index2 (tok : tokens)

   | (peek index 0) == '\n' || (peek index 0) == '\0'
   = tokenize src (index + 1) tokens

   | otherwise 
   = tokens
   where
      peek :: Int -> Int -> Char
      peek index offset =
         if (index + offset >= length src) || (index + offset < 0)
            then '\0'
            else src !! (index + offset)
