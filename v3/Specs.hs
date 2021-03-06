import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "a RPN calculator" $ do

        it "should allow for both numbers and message" $ do
            push 42 []   `shouldBe` (Right [42])
            err "foo" [] `shouldBe` (Left "foo - no result")

        it "should allow for multiple pushes" $ do
            (calc >>= push 4 >>= push 7) `shouldBe` (Right [7,4])

        it "should have an initial state" $ do
            calc `shouldBe` (Right [])

        it "should propagate error" $ do
            (calc >>= err "bar" >>= push 3) `shouldBe`
                (Left "bar - no result")

        it "should allow for unary operations" $ do
            (calc >>= push 5 >>= unary negate) `shouldBe`
                (Right [-5])

            (calc >>= push 6 >>= unary (+1)) `shouldBe`
                (Right [7])

        it "should allow for binary operation" $ do
            (calc >>= push 9 >>= push 7 >>= binary (-))
            `shouldBe` (Right [2])

        it "should inspect stack size" $ do
            (calc >>= inspect 1) `shouldBe` 
                (Left "not enough parameters - no result")
            (calc >>= push 3 >>= inspect 1) `shouldBe` (Right [3])
            (calc >>= push 1 >>= inspect 2) `shouldBe` 
                (Left "not enough parameters - no result")

        it "should inspect stack before operation" $ do
            (calc >>= unary negate) `shouldBe`
                (Left "not enough parameters - no result")
            (calc >>= binary (+)) `shouldBe`
                (Left "not enough parameters - no result")

        it "should evaluate a commande" $ do
            (calc >>= push 1 >>= cmd "neg") `shouldBe`
                (Right [-1])
               
              
           
       
