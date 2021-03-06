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
            calc "foo 43 neg" `shouldBe` "foo?"
        it "should check for missing parameters" $ do
            calc "neg" `shouldBe` "missing parameter"
            calc "   " `shouldBe` "missing parameter"
            calc "4 +" `shouldBe` "missing parameter"
            calc "  +" `shouldBe` "missing parameter"
        it "should allow for binary operation" $ do
            calc "23 17 +" `shouldBe` "40"
            calc "23 17 *" `shouldBe` "391"
            calc "42 5 -" `shouldBe` "37"
            calc "42 5 /" `shouldBe` "8"
        
