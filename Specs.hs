import Test.Hspec
import Calc (calc)

main = hspec $ do
    describe "calc" $ do
        it "can read a number" $ do
            calc "4807" `shouldBe` "4807"

        it "signals an error" $ do
            calc "foo" `shouldBe` "f ??"

        it "can read several numbers" $ do
            calc "17 42" `shouldBe` "42"

        it "cancels evaluation after an error" $ do
            calc "bar 66" `shouldBe` "b ??"

        it "allows for unary operation like negate" $ do
            calc "128 ~" `shouldBe` "-128"

        it "separates numbers and operators" $ do
            calc "321~" `shouldBe` "-321"

        it "allows for binary operation like addition" $ do
            calc "32 17+"  `shouldBe` "49"

        it "allows for multiplication" $ do
            calc "3 15*" `shouldBe` "45"

        it "allows for sequences of binary operations" $ do
            calc "2 3 4 *+"  `shouldBe` "14"
