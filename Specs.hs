import Test.Hspec
import Calc (calc)

main = hspec $ do
    describe "calc" $ do
        it "can read a number" $ do
            calc "4807" `shouldBe` "4807"


        it "signals an error" $ do
            calc "foo" `shouldBe` "foo ??"


        it "can read several numbers" $ do
            calc "17 42" `shouldBe` "42"


        it "cancels evaluation after an error" $ do
            calc "bar 66" `shouldBe` "bar ??"
