module lexer where

data TokenType
   = Const
   | Unsigned
   | Static
   | U8
   | I8
   | U16
   | I16
   | U32
   | I32
   | U64
   | I64
   | Let
   | Int
   | Char
   | Float
   | Double
   | Void
   | Struct
   | Enum
   | Union
   | If
   | Else
   | Switch
   | Case
   | Default
   | Typedef
   | Sizeof
   | Volatile
   
