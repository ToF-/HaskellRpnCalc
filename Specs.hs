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
            
        
