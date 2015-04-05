import Test.Hspec
import RPNCalculator

c = calculator
main = hspec $ do
    describe "RPN calculator" $ do
        it "should not yield a result when not given numbers" $ do
            result c `shouldBe` Left "stack is empty - no result"

        it "should yield the last number stacked as a result" $ do
            result (c -: Number 4807.00) `shouldBe` Right 4807.00
            result (c -: Number 1 -: Number 2) `shouldBe` (Right 2.0)

        it "should support addition" $ do
            result (c -: Number 1 -: Number 2 -: Binary (+)) `shouldBe` (Right 3.0)

        it "yields an error when not enough numbers on the stack for binary addition" $ do
            result (c -: Number 1 -: Binary (+)) `shouldBe` (Left "not enough parameters - no result")

        it "stops at the first error when given not enough numbers" $ do
            result (c -: Number 1 -: Binary (+) -: Number 2 -: Binary (*)) `shouldBe`(Left "not enough parameters - no result")

      


