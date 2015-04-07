import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "a RPN calculator" $ do
        it "should allow pushing numbers" $ do
            push 4807 [] `shouldBe` [4807]
        
