import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "RPN calculator" $ do
        it "should read a correct number" $ do
            eval "4807" `shouldBe` "4807"
