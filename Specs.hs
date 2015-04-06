import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "RPN calculator" $ do
        it "should read a correct number" $ do
            eval "4807" `shouldBe` "4807"
            eval "7084" `shouldBe` "7084"

        it "should signal an incorrect entry" $ do
            eval "foo" `shouldBe` "foo ? - no result"
            eval "bar" `shouldBe` "bar ? - no result"

        it "should output the top of the stack only" $ do
            eval "4807 6502" `shouldBe` "6502"

        it "should support an unary operation" $ do
            eval "4807 neg" `shouldBe` "-4807"
            eval "7084 neg" `shouldBe` "-7084"

        it "should support a binary operation" $ do
            eval "4000 807 +" `shouldBe`  "4807"
