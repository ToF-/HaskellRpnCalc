import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "RPN Calculator" $ do
        it "should push numbers on a stack" $ do
            push 4807 [] `shouldBe` Right [4807]
            (calc >>=
             push 1   >>=
             push 2   >>= 
             push 3) `shouldBe` Right [3,2,1]

        it "should propagate an error" $ do
            err "woot!" [] `shouldBe` Left "woot!"
            (calc  >>=
             err "foo" >>=
             push 42   >>=
             push 100) `shouldBe` Left "foo"

        it "should support unary operation" $ do
            (calc  >>=
             push 4807 >>=
             unary negate) `shouldBe` Right [-4807]

        it "should support binary operation" $ do
            (calc >>=
             push 4   >>=
             push 3   >>=
             binary (-)) `shouldBe` Right [1]

        it "should inspect the stack size" $ do
            (calc   >>=
             push 4 >>=
             binary (+)) `shouldBe` Left "not enough parameters - no result"
            (calc   >>=
             unary negate) `shouldBe` Left "not enough parameters - no result"

        it "should detect a division by zero" $ do
            (calc   >>=
             push 4 >>=
             push 0 >>=
             binary div) `shouldBe` Left "division by zero - no result"

        it "should convert a string into a command" $ do
            (calc   >>= push 4 >>= cmd "neg") `shouldBe` Right [-4]
            (calc   >>= push 4 >>= push 3 >>= cmd "+") `shouldBe` Right [7]
            (calc   >>= push 4 >>= push 3 >>= cmd "-") `shouldBe` Right [1]
            (calc   >>= push 4 >>= push 3 >>= cmd "*") `shouldBe` Right [12]
            (calc   >>= push 6 >>= push 3 >>= cmd "/") `shouldBe` Right [2]
              

            
