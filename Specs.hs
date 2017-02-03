import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "calc" $ do
        it "should evaluate a number" $ do
            calc "42" `shouldBe` "42"
            calc "-8" `shouldBe` "-8" 
        it "should allow unary operation" $ do
            calc "42 neg" `shouldBe` "-42"
