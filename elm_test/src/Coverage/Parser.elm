module Coverage.Parser where

import Combine exposing (Parser, between, or, parens, regex, sepBy, squareBrackets, string)
import Combine.Char exposing (char)
import Combine.Infix exposing ((<*>), (*>), (<$>))
import Combine.Num exposing (int)

import Coverage.Types exposing (Tix(Tix), TixModule(TixModule))

tix : Parser Tix
tix =
  whitespaced (string "Tix") *>
  (Tix <$> (parens (list tixModule) `or` list tixModule))

tixModule : Parser TixModule
tixModule =
  whitespaced (string "TixModule") *>
  (TixModule <$> whitespaced literalString
             <*> whitespaced int
             <*> whitespaced int
             <*> whitespaced (list int))

whitespace : Parser String
whitespace =
  regex "[ \t\r\n]*"

whitespaced : Parser a -> Parser a
whitespaced =
  between whitespace whitespace

list : Parser a -> Parser (List a)
list parser =
  squareBrackets (sepBy (whitespaced (char ',')) (whitespaced parser))

literalString : Parser String
literalString =
  between (char '"') (char '"') (regex "[^\"]*")
