module ParserSpec
    ( spec
    ) where


import           Expr       (Expr (..))

import           Parser     (parse)

import           Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Parser.parse" $ do

    -- NOTE: Tests in this file intentionally FAIL because the parser
    -- has bugs. The grammar in Parser.y doesn't specify:
    --   1. Operator precedence (multiplication should bind tighter than addition)
    --   2. Operator associativity (all operators should be left-associative)
    --
    -- Fix the bugs in the parser so that tests pass (and potentially add more tests of
    -- your own)!

    -- Basic literal parsing
    describe "integer literals" $ do
      it "parses zero" $
        parse "0" `shouldBe` Int 0

      it "parses single digit" $
        parse "5" `shouldBe` Int 5

      it "parses multiple digits" $
        parse "123" `shouldBe` Int 123

      it "parses large numbers" $
        parse "987654321" `shouldBe` Int 987654321

    -- Basic operator parsing
    describe "binary operators" $ do
      it "parses addition" $
        parse "1 + 2" `shouldBe` Plus (Int 1) (Int 2)

      it "parses multiplication" $
        parse "4 * 7" `shouldBe` Times (Int 4) (Int 7)

    -- Whitespace handling
    describe "whitespace handling" $ do
      it "handles no whitespace" $
        parse "1+2" `shouldBe` Plus (Int 1) (Int 2)

      it "handles multiple spaces" $
        parse "1   +   2" `shouldBe` Plus (Int 1) (Int 2)

    -- Parentheses
    describe "parentheses" $ do
      it "parses parenthesized expression" $
        parse "(1 + 2)" `shouldBe` Plus (Int 1) (Int 2)

      it "parses nested parentheses" $
        parse "((5))" `shouldBe` Int 5

      it "parses parentheses with operators" $
        parse "(1 + 2) * 3" `shouldBe` Times (Plus (Int 1) (Int 2)) (Int 3)

      it "respects parentheses for precedence override" $
        parse "2 * (3 + 4)" `shouldBe` Times (Int 2) (Plus (Int 3) (Int 4))

    -- Operator precedence tests
    describe "operator precedence" $ do
      it "multiplication has higher precedence than addition" $
        parse "1 * 2 + 3" `shouldBe` Plus (Times (Int 1) (Int 2)) (Int 3)

      it "respects precedence in complex expression" $
        parse "2 + 3 * 4 + 5" `shouldBe`
          Plus (Plus (Int 2) (Times (Int 3) (Int 4))) (Int 5)

    -- Operator associativity tests
    describe "operator associativity (should be left-associative)" $ do
      it "addition is left-associative" $
        parse "1 + 2 + 3" `shouldBe` Plus (Plus (Int 1) (Int 2)) (Int 3)

      it "multiplication is left-associative" $
        parse "2 * 3 * 4" `shouldBe` Times (Times (Int 2) (Int 3)) (Int 4)


    -- Complex expressions
    describe "complex expressions" $ do
      it "parses complex nested expression" $
        parse "(1 + 2) * (3 + 4)" `shouldBe`
          Times (Plus (Int 1) (Int 2)) (Plus (Int 3) (Int 4))

