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
            err "woot!" [] `shouldBe` Left "woot! - no result"
            (calc  >>=
             err "foo" >>=
             push 42   >>=
             push 100) `shouldBe` Left "foo - no result"

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

        it "should convert unkown string into number or err" $ do
            (calc >>= cmd "-4807") `shouldBe` Right [-4807]
            (calc >>= cmd "foo") `shouldBe` Left "foo ? - no result"

        it "should convert a string into a command" $ do
            (calc   >>= push 4 >>= cmd "neg") `shouldBe` Right [-4]
            (calc   >>= push 4 >>= push 3 >>= cmd "+") `shouldBe` Right [7]
            (calc   >>= push 4 >>= push 3 >>= cmd "-") `shouldBe` Right [1]
            (calc   >>= push 4 >>= push 3 >>= cmd "*") `shouldBe` Right [12]
            (calc   >>= push 6 >>= push 3 >>= cmd "/") `shouldBe` Right [2]
              
        it "should eval sequences of commands" $ do
            (calc >>= eval "4 3 *") `shouldBe` Right [12]
            (calc >>= eval "4 bar *") `shouldBe` Left "bar ? - no result"

        it "should show each result" $ do
            process calc ["4","3 2 *","+"] `shouldBe`
                ["Right [4]","Right [6,4]","Right [10]"]

        it "allows for clearing stack" $ do
            (calc >>= push 4 >>= clear) `shouldBe` (Right [])   
            (calc >>= eval "4 clear") `shouldBe` (Right [])
