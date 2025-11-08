module Lexer where

import Data.Char
import Control.Monad.State

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
   | Error
   deriving (Show, Eq)

data Value
   = FLoatV Float
   | IntV Int
   | CharV Char
   | StringV String
   | None
   deriving (Show, Eq)

data Token = Token {tokenType :: TokenType, value :: Value} | EmptyToken deriving (Show, Eq)

data TokState = TokState {src :: String, index :: Int, tokens :: [Token]}

type Tokenizer a = State TokState a

tokenize :: Tokenizer () -- returns the reversed list
tokenize = do
   st <- get
   let i = index st
       s = src st
   c0 <- peek 0
   c1 <- peek 1
   c2 <- peek 2
   case (c0, c1, c2) of
      ('0', 'x', _) -> handleHex ""
      ('0', 'o', _) -> handleOct ""
      ('0', 'b', _) -> handleBin ""
      ('"', _, _) -> handleString
      ('\'', _, _) -> handleChar
      ('\'', '\\', _) -> handleEscChar
      (c, _, _) | isDigit c -> handleNum
      (c, _, _) | isAlpha c -> handleWord
      ('/', '/', _) -> handleComm
      ('/', '*', _) -> handleMultComm
      (x, y, z) | fst (charExpr x y z) /= EmptyToken -> handleCharExpr
      ('\n', _, _) -> skipOne
      (c, _, _) | c /= '\0' -> skipOne
      (_, _, _) -> return ()



      
   {-| (peek index 0) == '0' && (peek index 1) == 'x'
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
   = tokens-}
   where
      peek :: Int -> Tokenizer Char
      peek offset = do
         st <- get
         let i = index st
             s = src st
         if (i + offset >= length s) || (i + offset < 0)
            then pure '\0'
            else pure (s !! (i + offset))

      skipOne :: Tokenizer ()
      skipOne = modify (\st -> st { index = index st + 1})

      handleComm :: Tokenizer ()
      handleComm = do
         c <- peek 0
         if c == '\n' || c == '\0'
            then return ()
            else skipOne >> handleComm

      handleMultComm :: Tokenizer ()
      handleMultComm = do
         skipOne
         ca <- peek 0
         cb <- peek 1
         if ca == '*' && cb == '/'
            then skipOne
            else handleMultComm

      charExpr :: Char -> Char -> Char -> (Token, Int)
      charExpr '<' '<' '=' = (Token LShifAssign None, 3)
      charExpr _ _ _ = (EmptyToken, -1)

      handleCharExpr :: Tokenizer ()
      handleCharExpr = do
         ch1 <- peek 0
         ch2 <- peek 1
         ch3 <- peek 2
         let (tok, len) = charExpr ch1 ch2 ch3
         modify (\st -> st { index = index st + len, tokens = tok : tokens st})

      handleHex :: String -> Tokenizer ()
      handleHex buf = do
         st <- get
         if isHexDigit (peekraw (src st) (index st))
            then skipOne >> handleHex (peekraw (src st) (index st) : buf)
            else modify (\st -> st { tokens = Token HexLit (IntV (read $ reverse ('0' : 'x' : buf))) : tokens st})

      handleBin :: String -> Tokenizer ()
      handleBin buf = do
         st <- get
         if isBinDigit (peekraw (src st) (index st))
            then skipOne >> handleBin (peekraw (src st) (index st) : buf)
            else modify (\st -> st { tokens = Token BinLit (IntV (read $ reverse ('0' : 'b' : buf))) : tokens st})

      handleOct :: String -> Tokenizer ()
      handleOct buf = do
         st <- get
         if isOctDigit (peekraw (src st) (index st))
            then skipOne >> handleOct (peekraw (src st) (index st) : buf)
            else modify (\st -> st { tokens = Token OctLit (IntV (read $ reverse ('0' : 'o' : buf))) : tokens st})

peekraw :: String -> Int -> Char
peekraw src idx =
   if idx > length src || idx < 0
      then '\0'
      else src !! idx

isBinDigit c = c == '0' || c == '1'


      {-completeNum :: Int -> String -> (Token, Int) -- does not work on floats for now
      completeNum idx buf = 
         if isDigit (peek idx 0)
            then let newbuf = (peek idx 0) : buf
                 in completeNum (idx + 1) newbuf
            else let int = read $ reverse buf
                 in (Token IntLit (IntV int), idx)

      completeBin :: Int -> String -> (String, Int)
      completeBin idx buf = 
         if (peek idx 0) == 0 || (peek idx 0) == 1
            then let newbuf = (peek idx 0) : buf
                 in completeBin (idx + 1) newbuf
            else (reverse buf, idx)


      completeOct :: Int -> String -> (String, Int)
      completeOct idx buf = 
         if  isOctDigit (peek idx 0)
            then let newbuf = (peek idx 0) : buf
                 in completeOct (idx + 1) newbuf
            else (reverse buf, idx)

      completeHex :: Int -> String -> (String, Int)
      completeHex idx buf = 
         if  isHexDigit (peek idx 0)
            then let newbuf = (peek idx 0) : buf
                 in completeHex (idx + 1) newbuf
            else (reverse buf, idx)-}

