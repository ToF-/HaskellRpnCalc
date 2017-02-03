import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "calc" $ do
        it "should evaluate a number" $ do
            calc "42" `shouldBe` "42"
            calc "-8" `shouldBe` "-8" 
