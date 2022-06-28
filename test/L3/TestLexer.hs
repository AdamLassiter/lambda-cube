module L3.TestLexer (tests) where

import L3.Lexer
import Test

tests :: [IO ()]
tests =
  [ testLexer
  ]

testLexer :: IO ()
testLexer = do
  assertEq "TestLexer" (lexSrc "") (Right []) "Lex no tokens"
  assertEq "TestLexer" (lexSrc "( )") (Right [OpenParen, CloseParen]) "Lex parens"

  assertEq "TestLexer" (lexSrc "* ⊤ # ⊥") (Right [StarT, StarT, BoxT, BoxT]) "Lex axioms"
  assertEq "TestLexer" (lexSrc ": ∈ . → ->") (Right [HasType, HasType, Arrow, Arrow, Arrow]) "Lex structural symbols"

  assertEq "TestLexer" (lexSrc "lambda ∃ λ forall ∀ π") (Right [LambdaT, LambdaT, LambdaT, PiT, PiT, PiT]) "Lex functions"
  assertEq "TestLexer" (lexSrc "@ -- a comment (-> * tokens)\n") (Right [At, Comment "a comment (-> * tokens)", EOL]) "Lex namespacing and whitespacing"

  assertEq "TestLexer" (lexSrc "A a Γ γ 1 0 -1") (Right [Symbol "A", Symbol "a", Symbol "Γ", Symbol "γ", Number 1, Number 0, Number $ -1]) "Lex numbers and symbols"
