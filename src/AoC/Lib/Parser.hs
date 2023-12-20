module AoC.Lib.Parser
  ( module X,
    Parser,
    intP,
    signedIntP,
    lexeme,
    strP,
    sc,
    intP0,
    signedIntP0,
    lexeme0,
    strP0,
    sc0,
    enumParser,
    maybeToP,
    predToP,
    parensP,
    bracesP,
    squareBracketsP,
    angleBracketsP,
    doubleQuotes,
    parensP0,
    bracesP0,
    squareBracketsP0,
    angleBracketsP0,
    doubleQuotes0,
  )
where

import AoC.Lib.Prelude
import Data.Char as X
import Text.Megaparsec as X hiding (Pos, State (..), parse)
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Parsec Void String

---------------------------------------------------------------------------
-- Space consuming versions

-- >>> parseMaybe (strP "abc") "abc  "
-- Just "abc"
strP :: String -> Parser String
strP = lexeme . string

-- >>> parseMaybe intP "123  "
-- Just 123
intP :: Parser Int
intP = lexeme Lexer.decimal

-- >>> parseMaybe signedIntP "- 123  "
-- Just (-123)
signedIntP :: Parser Int
signedIntP = Lexer.signed sc intP

-- >>> parseMaybe (parensP intP) "(  123  )  "
-- Just 123
parensP, bracesP, squareBracketsP, angleBracketsP, doubleQuotes :: Parser a -> Parser a
parensP = "(" `inside` ")"
bracesP = "{" `inside` "}"
squareBracketsP = "[" `inside` "]"
angleBracketsP = "<" `inside` ">"
doubleQuotes = "\"" `inside` "\""

inside :: String -> String -> Parser a -> Parser a
inside open close = (strP open <* sc) `between` (sc *> strP close)

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

sc :: Parser ()
sc = Lexer.space hspace1 empty empty

---------------------------------------------------------------------------
-- Non-space consuming versions

-- >>> (parseMaybe (strP0 "abc") "abc", parseMaybe (strP0 "abc") "abc ")
-- (Just "abc",Nothing)
strP0 :: String -> Parser String
strP0 = lexeme0 . string

-- >>> parseMaybe intP0 "123"
-- Just 123
intP0 :: Parser Int
intP0 = lexeme0 Lexer.decimal

-- >>> parseMaybe signedIntP0 "- 123"
-- Just (-123)
signedIntP0 :: Parser Int
signedIntP0 = Lexer.signed sc intP0

-- >>> parseMaybe (parensP0 intP0) "(  123  )"
-- Just 123
parensP0, bracesP0, squareBracketsP0, angleBracketsP0, doubleQuotes0 :: Parser a -> Parser a
parensP0 = "(" `inside0` ")"
bracesP0 = "{" `inside0` "}"
squareBracketsP0 = "[" `inside0` "]"
angleBracketsP0 = "<" `inside0` ">"
doubleQuotes0 = "\"" `inside0` "\""

inside0 :: String -> String -> Parser a -> Parser a
inside0 open close = (strP0 open <* sc) `between` (sc *> strP0 close)

lexeme0 :: Parser a -> Parser a
lexeme0 = Lexer.lexeme sc0

sc0 :: Parser ()
sc0 = Lexer.space empty empty empty

---------------------------------------------------------------------------
-- Helpers

maybeToP :: (a -> Maybe b) -> a -> Parser b
maybeToP f = maybe empty pure . f

predToP :: (a -> Bool) -> a -> Parser a
predToP p x = if p x then pure x else empty

{-
boolP :: Parser Bool
boolP = enumParser show $ \case
  "False" -> Just False
  "True" -> Just True
  _ -> Nothing
-}
enumParser ::
  forall a.
  (Bounded a, Enum a) =>
  (a -> String) ->
  (String -> Maybe a) ->
  Parser a
enumParser printer parser = do
  s <- choice $ map (string . printer) (enumerate @a)
  maybeToP parser s
