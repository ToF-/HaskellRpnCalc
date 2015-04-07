import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "a RPN calculator" $ do

        it "should allow for both numbers and message" $ do
            push 42 []   `shouldBe` (Right [42])
            err "bar" [] `shouldBe` (Left "foo - no result")
            
        
