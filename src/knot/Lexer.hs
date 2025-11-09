module Lexer where

import Data.Char
import qualified Text.Read as Read
import Control.Monad.State

data TokenType
   = Identifier
   | Mutable
   | Inline
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
   | LShiftAssign
   | RShift
   | LShift
   | QMark
   | Return
   | For
   | Error
   deriving (Show, Eq)

data Value
   = FloatV Float
   | IntV Int
   | CharV Char
   | StringV String
   | NameV String
   | None
   deriving (Show, Eq)


data Token = Token {tokenType :: TokenType, value :: Value} | EmptyToken deriving (Show, Eq)

data TokState = TokState {src :: String, index :: Int, tokens :: [Token]}

type Tokenizer a = State TokState a

tokenize :: Tokenizer () -- returns the reversed list
tokenize = do
   st <- get
   c0 <- peek 0
   c1 <- peek 1
   c2 <- peek 2
   case (c0, c1, c2) of
      ('0', 'x', _) -> handleHex "" >> tokenize
      ('0', 'o', _) -> handleOct "" >> tokenize
      ('0', 'b', _) -> handleBin "" >> tokenize
      ('"', _, _) -> handleString "" >> tokenize
      ('\'', c, '\'') -> handleChar c >> tokenize
      ('\'', '\\', _) -> handleEscChar "" >> tokenize
      (c, _, _) | isDigit c -> handleNum "" >> tokenize
      (c, _, _) | isAlpha c -> handleWord "" >> tokenize
      ('/', '/', _) -> handleComm >> tokenize
      ('/', '*', _) -> handleMultComm >> tokenize
      (x, y, z) | fst (charExpr x y z) /= EmptyToken -> handleCharExpr >> tokenize
      ('\n', _, _) -> skipOne >> tokenize
      (c, _, _) | c /= '\0' -> skipOne >> tokenize
      (_, _, _) -> return ()

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
      skipOne = do
         st <- get
         modify (\st -> st { index = index st + 1})

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

      handleChar :: Char -> Tokenizer ()
      handleChar c = do
         st <- get
         modify (\st -> st { index = index st + 3, tokens = Token CharLit (CharV c) : tokens st})

      handleEscChar :: String -> Tokenizer ()
      handleEscChar buf = do
         skipOne
         chr <- peek 0
         if chr /= '\'' && chr /= '\0'
            then handleEscChar (chr : buf)
            else modify (\st -> st {tokens = Token CharLit (StringV $ reverse buf) : tokens st})

      handleWord :: String -> Tokenizer ()
      handleWord buf = do
         chr <- peek 0
         if isAlphaNum chr
            then skipOne >> handleWord (chr : buf)
            else identifyKeywords (reverse buf)

      handleString :: String -> Tokenizer ()
      handleString buf = do
         skipOne
         chr <- peek 0
         if chr /= '"' && chr /= '\0'
            then handleString (chr : buf)
            else modify (\st -> st { tokens = Token StrLit (StringV $ reverse buf) : tokens st})

      handleNum :: String -> Tokenizer ()
      handleNum buf = do
         st <- get
         if isFloatDigit (peekraw (src st) (index st))
            then skipOne >> handleNum (peekraw (src st) (index st) : buf)
            else saveNum (reverse buf)

      saveNum :: String -> Tokenizer ()
      saveNum buf
         | not (isVFLoat buf)
         = modify (\st -> st { tokens = Token Error (StringV ("Error: " ++ buf ++ " is not a valid Number")) : tokens st})

         | isFloat buf
         = modify (\st -> st { tokens = Token FloatLit (FloatV (read buf)) : tokens st})

         | otherwise
         = modify (\st -> st { tokens = Token IntLit (IntV (read buf)) : tokens st})

      identifyKeywords :: String -> Tokenizer ()
      identifyKeywords "fn" = modify (\st -> st { tokens = Token Function None : tokens st})
      identifyKeywords "char" = modify (\st -> st { tokens = Token CharT None : tokens st})
      identifyKeywords buf = modify (\st -> st { tokens = Token Identifier (NameV buf) : tokens st})

      charExpr :: Char -> Char -> Char -> (Token, Int)
      charExpr '<' '<' '=' = (Token LShiftAssign None, 3)
      charExpr '>' '>' '=' = (Token RShiftAssign None, 3)
      charExpr '<' '<' _ = (Token LShift None, 2)
      charExpr '>' '>' _ = (Token RShift None, 2)
      charExpr '=' '=' _ = (Token Equals None, 2)
      charExpr '<' '=' _ = (Token Lequals None, 2) 
      charExpr '>' '=' _ = (Token Grequals None, 2)
      charExpr '!' '=' _ = (Token Nequals None, 2)
      charExpr '=' '>' _ = (Token DoubleArrow None, 2)
      charExpr '-' '>' _ = (Token SingleArrow None, 2)
      charExpr '+' '=' _ = (Token AddAssign None, 2)
      charExpr '-' '=' _ = (Token SubAssign None, 2)
      charExpr '*' '=' _ = (Token MultAssign None, 2)
      charExpr '/' '=' _ = (Token DivAssign None, 2)
      charExpr '%' '=' _ = (Token ModAssign None, 2)
      charExpr '&' '=' _ = (Token AndAssign None, 2)
      charExpr '|' '=' _ = (Token OrAssign None, 2)
      charExpr '^' '=' _ = (Token XOrAssign None, 2)
      charExpr '+' '+' _ = (Token Increment None, 2)
      charExpr '-' '-' _ = (Token Decrement None, 2)
      charExpr '+' _ _ = (Token Plus None, 1)
      charExpr '-' _ _ = (Token Minus None, 1)
      charExpr '*' _ _ = (Token Star None, 1)
      charExpr '/' _ _ = (Token FSlash None, 1)
      charExpr '\\' _ _ = (Token BSlash None, 1)
      charExpr '%' _ _ = (Token Modulo None, 1)
      charExpr '=' _ _ = (Token Equal None, 1)
      charExpr _ _ _ = (EmptyToken, -1)




peekraw :: String -> Int -> Char
peekraw src idx =
   if idx > length src || idx < 0
      then '\0'
      else src !! idx

isBinDigit c = c == '0' || c == '1'
isFloatDigit c = isDigit c || elem c ".eE+-"
isVFLoat f = case Read.readMaybe f :: Maybe Double of
   Just _ -> True
   Nothing -> False
isFloat f = elem '.' f || elem 'e' f || elem 'E' f
