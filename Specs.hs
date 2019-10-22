import Test.Hspec
import RpnCalc

main = hspec $ do
    describe "eval" $ do
        describe "can evaluate tokens using a stack:" $ do
            it "const pushes a constant on the stack" $ do
                []   `eval` (Const 42) `shouldBe` [42]
                []   `eval` (Const 17) `shouldBe` [17]
                [42] `eval` (Const 17) `shouldBe` [17,42]

            it "unary applies a unary operation on the top of the stack" $ do 
                [42] `eval` (Unary negate) `shouldBe` [-42]

            it "binary applies a binary operation on the top and second to top of the stack" $ do 
                [17,42] `eval` (Binary (+)) `shouldBe` [59]

    describe "parse" $ do
        describe "can parse a string into tokens" $ do
            it "parses a string into a constant number" $ do
                let [(Const n,_)] = parse "42"
                n  `shouldBe` 42

