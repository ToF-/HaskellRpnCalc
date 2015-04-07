import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "RPN Calculator" $ do
        it "should push numbers on a stack" $ do
            push 4807 [] `shouldBe` Right [4807]
            (Right [] >>=
             push 1   >>=
             push 2   >>= 
             push 3) `shouldBe` Right [3,2,1]

        it "should propagate an error" $ do
            err "woot!" [] `shouldBe` Left "woot!"
            (Right []  >>=
             err "foo" >>=
             push 42   >>=
             push 100) `shouldBe` Left "foo"

        it "should support unary operation" $ do
            (Right []  >>=
             push 4807 >>=
             unary negate) `shouldBe` Right [-4807]

        it "should support binary operation" $ do
            (Right [] >>=
             push 4   >>=
             push 3   >>=
             binary (-)) `shouldBe` Right [1]
