module Json ( isNextCharacter 
            , isFirstCharacterOpenCurlyBracket
            , isFirstCharacterOpenSquareBracket
            , isLastCharacterClosedCurlyBracket
            , isLastCharacterClosedSquareBracket
            , isStartingCurlyBrackets
            , isStartingSquareBrackets
            , isEitherStartingCurlyOrSquareBrackets 
            , isCorrectCharactersAfterComma 
            , isValidJson 
            ) where
import Query (tail)
import Prim (Boolean(..), Array(..), String(..))
import Data.Array (concat, head, reverse, all, any, length)
import Data.String (Pattern(..), split)
import Data.Function (($))
import Data.Ord ((<))
import Data.Maybe (Maybe(..))
import Data.Eq ((==))
import Data.Boolean (otherwise)

isNextCharacter :: Array String -> Boolean
isNextCharacter xs | length xs < 2 = false
                   | Just " " == head (tail xs) = isNextCharacter' $ head xs
                       where
                         isNextCharacter' :: Maybe String -> Boolean
                         isNextCharacter' Nothing = false
                         isNextCharacter' (Just x) = isNextCharacter $ concat [[x], tail ( tail xs)]
                   | otherwise = head xs == head (tail xs)

isFirstCharacterOpenCurlyBracket :: Array String -> Boolean
isFirstCharacterOpenCurlyBracket xs = isNextCharacter $ concat [["{"], xs]

isFirstCharacterOpenSquareBracket :: Array String -> Boolean
isFirstCharacterOpenSquareBracket xs = isNextCharacter $ concat [["["], xs]

isLastCharacterClosedCurlyBracket :: Array String -> Boolean
isLastCharacterClosedCurlyBracket xs = isNextCharacter $ concat [["}"], reverse xs]

isLastCharacterClosedSquareBracket :: Array String -> Boolean
isLastCharacterClosedSquareBracket xs = isNextCharacter $ concat [["]"], reverse xs]

isStartingCurlyBrackets :: Array String -> Boolean
isStartingCurlyBrackets xs = all (\ it -> it) [ isFirstCharacterOpenCurlyBracket xs
                                              , isLastCharacterClosedCurlyBracket xs]

isStartingSquareBrackets :: Array String -> Boolean
isStartingSquareBrackets xs = all (\ it -> it) [ isFirstCharacterOpenSquareBracket xs
                                               , isLastCharacterClosedSquareBracket xs]

isEitherStartingCurlyOrSquareBrackets :: Array String -> Boolean
isEitherStartingCurlyOrSquareBrackets xs = any (\ it -> it) [ isStartingCurlyBrackets xs
                                                            , isStartingSquareBrackets xs]

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

isValidJson :: String -> Boolean
isValidJson s = all (\ it -> it) [ isEitherStartingCurlyOrSquareBrackets xs
                                 , isCorrectCharactersAfterComma xs]
  where
    xs = split (Pattern "") s
