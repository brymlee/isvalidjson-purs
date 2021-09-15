module IsCorrectCharactersAfterComma (isCorrectCharactersAfterComma) where

import Query (tail)
import Data.Maybe (Maybe(..))
import Data.Array (head)
import Prim (Boolean(..), Array(..), String(..))

type IsEscaped = Boolean

data Context = Free
             | Comma
             | Quotes IsEscaped 

isCorrectCharactersAfterComma :: Array String -> Boolean
isCorrectCharactersAfterComma xs' = f Free (head xs') (tail xs') 
  where
    free :: Array String -> Boolean
    free xs = f Free (head xs) (tail xs)
    f :: Context -> Maybe String -> Array String -> Boolean
    f Free (Just ",") xs = f Comma (head xs) (tail xs)
    f Free (Just _) xs = free xs
    f Comma (Just "\"") xs = f (Quotes false) (head xs) (tail xs)
    f Comma (Just "{") xs = free xs
    f Comma (Just "[") xs = free xs
    f Comma (Just "t") xs = free xs
    f Comma (Just "f") xs = free xs
    f Comma (Just "1") xs = free xs
    f Comma (Just "2") xs = free xs
    f Comma (Just "3") xs = free xs
    f Comma (Just "4") xs = free xs
    f Comma (Just "5") xs = free xs
    f Comma (Just "6") xs = free xs
    f Comma (Just "7") xs = free xs
    f Comma (Just "8") xs = free xs
    f Comma (Just "9") xs = free xs
    f Comma (Just " ") xs = f Comma (head xs) (tail xs)
    f Comma (Just _) _ = false
    f Comma Nothing _ = false
    f (Quotes _) (Just "\\") xs = f (Quotes true) (head xs) (tail xs) 
    f (Quotes true) (Just "\"") xs = f (Quotes false) (head xs) (tail xs)
    f (Quotes false) (Just "\"") xs = free xs
    f (Quotes _) (Just _) xs = f (Quotes false) (head xs) (tail xs)
    f _ Nothing _ = true
