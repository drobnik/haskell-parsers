module Parser.Primitives (
    Parser(..),

      char, word,
        number, literal, token,
          iter, while, cons,

            (<|>), (<=>), (>>>), (?>>>), (>>>=),
              (<+>), (<-+>), (<+->), (<?+>), (<+?>)
                                              ) where

import Control.Monad
import Data.Char

type Parser a = String -> Maybe (String, a)

-- Parsing of a single letter
char :: Parser Char
char []     = Nothing
char (c:cs) = Just (cs, c)

-- Parses a value m and returns a parser if a predicate is satisfied
infix 7 <=>
(<=>) :: Parser a -> (a -> Bool) -> Parser a
(m <=> p) cs =
    case m cs of
        Nothing -> Nothing
        Just (cs', a) -> if p a
                     then return (cs', a)
                     else Nothing

letter :: Parser Char
letter = char <=> isLetter

space :: Parser Char
space = char <=> (\x -> (x == ' '))

result :: a -> Parser a
result a b =  Just(b, a)

failure :: Parser a
failure = \_ -> Nothing


-- OR parser
infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(m <|> n) cs =
  case m cs of
  Nothing      -> n cs
  Just(cs', a) -> return (cs', a)


-- AND parser
infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(m <+> n) cs =
  case m cs of
    Nothing      -> Nothing
    Just(cs', a) -> case n cs' of
                  Just(cs'', b) -> Just(cs'', (a, b))
                  Nothing      -> Nothing


infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(m >>> f) cs =
  case m cs of
   Nothing      -> Nothing
   Just(cs', a)  -> Just (cs', f a )

cons :: (a, [a]) -> [a]
cons (hd, tl) = hd : tl

iter :: Parser a -> Parser [a]
iter m = m <+> iter m >>> cons <|> result []

-- iteration of the parser given predicate
while :: (Char -> Bool) -> Parser [Char]
while p = iter (char <=> p)


word :: Parser String
word = while (not . isSpace)

digit :: Parser Int
digit = char <=> isDigit >>> digitToInt

-- parsing of a number
buildNumber :: Int -> Int -> Int
buildNumber a b = a * 10 + b

-- function application
infixl 4 >>>=
(>>>=) :: Parser a -> (a -> Parser b) -> Parser b
(m >>>= n) cs = case m cs of
           Nothing      -> Nothing
           Just(cs', a) -> case n a cs' of
                     Nothing      -> Nothing
                     Just(cs'', b) -> Just(cs'', b)

-- number parser
number :: Parser Int
number = digit >>>= number'


-- Given the number already parsed, tries to parse next digit. If successes,
-- a new number will be constructed and parsing will continue recursively.
-- If not, a function will return already parsed value.

number' :: Int -> Parser Int
number' m = digit >>> buildNumber m >>>= number' <|> result m
--number' m = digit >>>= (\n -> number' (buildNumber m n)) <|> result m

literal :: Char -> Parser Char
literal c = char <=> (\x -> x == c)

-- Parser for particular word
token :: String -> Parser String
token []     = result []
token (c:cs) = literal c <+> token cs >>> cons

-- iteration variant -- reject empty sequences
iter' :: Parser a -> Parser [a]
iter' m = m <+> iter m >>> cons <|> failure


infixl 5 ?>>>
(?>>>) :: Parser a -> (a -> Maybe b) -> Parser b
(m ?>>> f) cs =  do
  (cs', a) <- m cs
  b        <- f a
  return (cs', b)

-- Ignores the output of the first parser and returns the second. If any parsing
-- fails -> fail
infixl 6 <-+>
(<-+>) :: Parser a -> Parser b -> Parser b
(m <-+> n) cs = case m cs of
           Nothing      -> Nothing
           Just(cs', a) -> case n cs of
             Nothing      -> Nothing
             Just(cs', b) -> Just(cs', b)

infixl 6 <+->
(<+->) :: Parser a -> Parser b -> Parser a
(m <+-> n) cs = case m cs of
           Nothing      -> Nothing
           Just(cs', a) -> case n cs of
             Nothing     -> Nothing
             Just(cs, b) -> Just(cs,  a)

-- Similar to <-+> but failure of the first parser is acceptable
infixl 6 <?+>
(<?+>) :: Parser a -> Parser b -> Parser b
(m <?+> n) cs = case m cs of
              _ -> case n cs of
                Nothing     -> Nothing
                Just(cs, b) -> Just(cs, b)

-- Similar to <+-> but failure of the second parser is acceptable
infixl 6 <+?>
(<+?>) :: Parser a -> Parser b -> Parser a
(m <+?> n) cs = case m cs of
           Nothing     -> Nothing
           Just(cs, a) -> case n cs of
                    _  -> Just(cs, a)
