import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "RPN Calculator" $ do
        it "should push numbers on a stack" $ do
            push 4807 [] `shouldBe` Right [4807]
            (Right [] >>=
             push 1   >>=
             push 2   >>= 
             push 3) `shouldBe` Right [3,2,1]
