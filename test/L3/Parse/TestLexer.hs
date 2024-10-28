module L3.Parse.TestLexer (tests) where

import L3.Parse
import Test

tests :: [IO ()]
tests =
  [ testLexer
  ]

testLexer :: IO ()
testLexer = do
  assertEq "Parse::TestLexer" (lexSrc "") (Right []) "Lex no tokens"
  assertEq "Parse::TestLexer" (lexSrc "( )") (Right [OpenParen, CloseParen]) "Lex parens"

  assertEq "Parse::TestLexer" (lexSrc "* ⊤ # ⊥") (Right [StarT, StarT, BoxT, BoxT]) "Lex axioms"
  assertEq "Parse::TestLexer" (lexSrc ": ∈ . → ->") (Right [HasType, HasType, Arrow, Arrow, Arrow]) "Lex structural symbols"

  assertEq "Parse::TestLexer" (lexSrc "∃ λ ∀ π") (Right [LambdaT, LambdaT, PiT, PiT]) "Lex functions"
  assertEq "Parse::TestLexer" (lexSrc "@ -- a comment (-> * tokens)\n") (Right [At, Comment "a comment (-> * tokens)", EOL]) "Lex namespacing and whitespacing"

  assertEq "Parse::TestLexer" (lexSrc "A a Γ γ 1 0 -1") (Right [Symbol "A", Symbol "a", Symbol "Γ", Symbol "γ", Number 1, Number 0, Number $ -1]) "Lex numbers and symbols"
