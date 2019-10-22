import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "eval" $ do
        describe "can evaluate tokens using a stack:" $ do
            it "const pushes a constant on the stack" $ do
                [] `eval` (Const 42) `shouldBe` [42]
                [] `eval` (Const 17) `shouldBe` [17]
                [42] `eval` (Const 17) `shouldBe` [17,42]

            it "unary applies a unary operation on the top of the stack" $ do 
                [42] `eval` (Unary negate) `shouldBe` [-42]
