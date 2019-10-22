import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "eval" $ do
        describe "can eval tokens using a stack:" $ do
            it "const pushes a constant on the stack" $ do
                [] `eval` (Const 42) `shouldBe` [42]
                [] `eval` (Const 17) `shouldBe` [17]
                [42] `eval` (Const 17) `shouldBe` [17,42]
