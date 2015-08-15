import Test.Hspec
import Calc (calc)

main = hspec $ do
    describe "calc" $ do
        it "can read a number" $ do
            calc "4807" `shouldBe` "4807"
