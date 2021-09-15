module Json ( isFirstCharacterOpenCurlyBracket 
            , isFirstCharacterOpenSquareBracket
            , isLastCharacterClosedCurlyBracket
            , isLastCharacterClosedSquareBracket
            , isStartingCurlyBrackets
            , isStartingSquareBrackets
            , isEitherStartingCurlyOrSquareBrackets 
            , isValidJson 
            ) where
import Prim (Boolean(..), Array(..), String(..))
import Data.Array (concat, reverse, all, any)
import Data.String (Pattern(..), split)
import Data.Function (($))
import IsCorrectCharactersAfterComma (isCorrectCharactersAfterComma)
import IsNextCharacter (isNextCharacter)

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

isValidJson :: String -> Boolean
isValidJson s = all (\ it -> it) [ isEitherStartingCurlyOrSquareBrackets xs
                                 , isCorrectCharactersAfterComma xs]
  where
    xs = split (Pattern "") s
