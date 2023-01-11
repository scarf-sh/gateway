{-# LANGUAGE OverloadedStrings #-}

module Scarf.Gateway.ImagePatternTest (testTree) where

import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Scarf.Gateway.ImagePattern
  ( Pattern,
    fromText,
    isConstPattern,
    match,
    toText,
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, testCase, (@=?), (@?=))

fromText_ :: Text -> Pattern
fromText_ =
  fromJust . fromText

test_print_parse_roundtrip :: TestTree
test_print_parse_roundtrip = testCase "Parse/Print roundtripping" $ do
  let examples =
        [ "linkerd/*",
          "lin{t, ker}/*",
          "a*b*c"
        ]

  for_ examples $ \example -> do
    example @=? toText (fromText_ example)

test_const_pattern :: TestTree
test_const_pattern = testCase "isConstPattern" $ do
  isConstPattern (fromText_ "linkerd/*") @?= Nothing
  isConstPattern (fromText_ "linkerd/{a,c}") @?= Nothing
  isConstPattern (fromText_ "blah/blah") @?= Just "blah/blah"
  isConstPattern (fromText_ "{abc}/{cdef}") @?= Just "abc/cdef"
  isConstPattern (fromText_ "\\{") @?= Just "{"
  isConstPattern (fromText_ "\\*/a/b/c") @?= Just "*/a/b/c"

test_match_1 :: TestTree
test_match_1 = testCase "Match 1" $ do
  let p = fromText_ "linkerd/*"

  assertBool "" (match p ["linkerd", "hello"])
  assertBool "" (match p ["linkerd", "hello{abc}"])
  assertBool "" $ not (match p [])
  assertBool "" $ not (match p ["linkerd"])
  assertBool "" $ not (match p ["linkerd", "hello", "world"])

  let p = fromText_ "l*n*k*er/d{1,2,3,4,5}"
  assertBool "" (match p ["linker", "d5"])

testTree :: IO [TestTree]
testTree =
  pure
    [ test_print_parse_roundtrip,
      test_const_pattern,
      test_match_1
    ]
