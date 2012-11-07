module Regexp where

import Control.Arrow (first, second)
import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Char

data RegExp
     = Symbol (Char -> Bool)
     | Union RegExp RegExp -- Union [RegExp]
     | Concat RegExp RegExp -- Concat [RegExp]
     | Star RegExp
     | Empty

type Remainder = String
type Success = Bool

type RegExpState = State (Remainder, Success)

char :: Char -> RegExp
char = Symbol . (==)

letter :: RegExp
letter = Symbol isAlpha

string :: String -> RegExp
string = foldr (Concat . char) empty

star :: RegExp -> RegExp
star = Star

(<.>) :: RegExp -> RegExp -> RegExp
(<.>) = Concat

(<+>) :: RegExp -> RegExp -> RegExp
(<+>) = Union

dot :: RegExp
dot = Symbol (const True)

minOne :: RegExp -> RegExp
minOne r = r <.> star r

empty :: RegExp
empty = Empty

nothing :: RegExp
nothing = Symbol (const False)

alphaNum :: RegExp
alphaNum = Symbol isAlphaNum

match' :: RegExp -> String -> RegExpState String
match' Empty s = successful True >> return s
match' (Symbol _) "" = successful False >> return ""
match' (Symbol p) a@(x:xs)
  | p x = sucRem True xs >> return [x]
  | otherwise = sucRem False a >> return ""
match' (Union r r') s = do
  a <- match' r s
  (rem, suc) <- get
  a' <- match' r' s
  (_, suc') <- get
  if suc && not suc'
     then sucRem suc rem >> return a
     else return a'
match' (Concat r r') s = do
  a <- match' r s
  (rem, suc) <- get
  if suc || s /= rem
     then (a ++) <$> match' r' rem
     else return a
match' (Star _) "" = sucRem True "" >> return ""
match' (Star r) s = do
  a <- match' r s
  (rem, suc) <- get
  if suc
     then (a ++) <$> match' (star r) rem
     else sucRem suc rem >> return a

sucRem s r = successful s >> setRemainder r
successful = modify . second . const
setRemainder = modify . first . const
              
match :: RegExp -> String -> (String, Remainder, Success)
match r s = case runState (match' r s) ("", True) of
  (matched, (remainder, s)) -> (matched, remainder, s)

-- tests
email :: RegExp
email = minOne alphaNum <.> char '@' <.> minOne alphaNum <.> string ".com"

test = minOne (char 'a') <.> char '@'
  
