import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "calc" $ do
        it "should evaluate a number" $ do
            calc "42" `shouldBe` "42"
            calc "-8" `shouldBe` "-8" 
        it "should allow unary operation" $ do
            calc "42 neg" `shouldBe` "-42"
            calc "-5 abs" `shouldBe` "5"
        it "should combine operations" $ do
            calc "42 neg abs" `shouldBe` "42"
        it "should deal with errors nicely" $ do
            calc "foo" `shouldBe` "foo?"
