import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "eval" $ do
        describe "can eval tokens using a stack:" $ do
            it "const" $ do
                [] `eval` (Const 42) `shouldBe` [42]
