import Test.Hspec
import RPNCalculator

c :: Calculator
c = calculator
main = hspec $ do
    describe "RPN calculator" $ do
        it "should not yield a result when not given numbers" $ do
            (c >>= pop) `shouldBe` Left "stack is empty - no result"

        it "should yield the last number stacked as a result" $ do
            (c >>= push 4807.00 >>= pop) `shouldBe` Right 4807.00
            (c >>= push 1 >>= push 2 >>= pop) `shouldBe` (Right 2.0)

        it "should support addition" $ do
            (c >>= push 1 >>= push 2 >>= binary (+) >>= pop) `shouldBe` (Right 3.0)

        it "should support subtraction" $ do
            (c >>= push 1 >>= push 2 >>= binary (-) >>= pop) `shouldBe` (Right (-1.0))

        it "yields an error when not enough numbers on the stack for binary operation" $ do
            (c >>= push 1 >>= binary (+) >>= pop) `shouldBe` (Left "not enough parameters - no result")

        it "stops at the first error when given not enough numbers" $ do
            (c >>= push 1 >>= binary (+) >>= push 2 >>= binary (*) >>= pop) `shouldBe`(Left "not enough parameters - no result")

        it "should support unary operation" $ do
            (c >>= push 48.05 >>= unary (negate) >>= pop) `shouldBe` (Right (-48.05))

        it "yields an error when not enough numbers for unary operation" $ do
            (c >>= unary (negate) >>= pop) `shouldBe` (Left "not enough parameters - no result")

        it "yields an error when trying to divide by 0" $ do
            (c >>= push 1 >>= push 0 >>= binary (/) >>= pop) `shouldBe` (Left "division by zero - no result")


 


