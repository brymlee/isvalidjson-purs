module IsNextCharacter (isNextCharacter) where

import Data.Ordering (Ordering(..))
import Prim (Boolean(..), Array(..), String(..))
import Data.Array (concat, head, length)
import Data.Maybe (Maybe(..))
import Query (tail)
import Data.Ord (compare)
import Data.Eq ((==))
import Data.Function (($))

isNextCharacter :: Array String -> Boolean
isNextCharacter xs = f $ compare (length xs) 2
  where
    f :: Ordering -> Boolean 
    f LT = false
    f _ = f' ((Just " ") == head (tail xs))
    f' :: Boolean -> Boolean
    f' true = f'' $ head xs
    f' false = head xs == head (tail xs) 
    f'' :: Maybe String -> Boolean
    f'' Nothing = false
    f'' (Just x) = isNextCharacter $ concat [[x], tail (tail xs)]
