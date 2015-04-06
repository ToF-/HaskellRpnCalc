import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "RPN calculator" $ do
        it "should read a correct number" $ do
            eval "4807" `shouldBe` "4807"

        it "should signal an incorrect entry" $ do
            eval "foo" `shouldBe` "foo ? - no result"
