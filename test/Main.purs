module Test.Main (main) where

import Control.Bind (discard)
import Data.Unit (Unit)
import Effect (Effect)
import Json ( isFirstCharacterOpenCurlyBracket
            , isFirstCharacterOpenSquareBracket
            , isLastCharacterClosedCurlyBracket
            , isStartingCurlyBrackets
            , isStartingSquareBrackets
            , isEitherStartingCurlyOrSquareBrackets 
            , isValidJson 
            ) 
import Test.Assert (assert)
import Data.Function (($))
import Prim (Boolean(..))
import Data.Eq ((==))
import IsNextCharacter (isNextCharacter)

main :: Effect Unit
main = do
  assert $ isNextCharacter ["{", "{"] 
  assert $ isNextCharacter ["{", "["] == false
  assert $ isNextCharacter ["{", " ", "{"]
  assert $ isNextCharacter ["a", " ", " ", "b"] == false
  assert $ isFirstCharacterOpenCurlyBracket [" ", " ", "{"]
  assert $ isFirstCharacterOpenCurlyBracket [" ", " ", "["] == false
  assert $ isFirstCharacterOpenSquareBracket [" ", " ", "["]
  assert $ isFirstCharacterOpenSquareBracket [" ", " ", "{"] == false
  assert $ isLastCharacterClosedCurlyBracket [" ", "[", "}"]
  assert $ isLastCharacterClosedCurlyBracket [" ", "{", "]"] == false
  assert $ isStartingCurlyBrackets [" ", "{", "a", "}", " "]
  assert $ isStartingCurlyBrackets [" ", "{", "b", "]", " "] == false
  assert $ isStartingSquareBrackets [" ", "[", "b", "]", " "]
  assert $ isStartingSquareBrackets [" ", "{", "c", "]", " "] == false
  assert $ isEitherStartingCurlyOrSquareBrackets [" ", "[", "a", "]", " "]
  assert $ isEitherStartingCurlyOrSquareBrackets [" ", "{", "a", "}", " "]
  assert $ isEitherStartingCurlyOrSquareBrackets [" ", "[", "a", "}", " "] == false
  assert $ isValidJson " {  } "
  assert $ isValidJson " [ ]"
  assert $ isValidJson "b" == false
  assert $ isValidJson "{ \"a\": \"b\", \"c\": \"d\" }"
  assert $ isValidJson "{ \"a\": \"b\", \"c\": \"d\", }" == false
  assert $ isValidJson "{ \"a\": \"b\",, }" == false
--  assert $ isValidJson "{ \"a\": \"b\", { }}" == false
