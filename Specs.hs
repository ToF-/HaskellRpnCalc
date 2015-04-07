import Test.Hspec
import RPNCalculator

main = hspec $ do
    describe "a RPN calculator" $ do
        it "should allow pushing numbers" $ do
            push 4807 [] `shouldBe` [4807]
        
        it "should allow for errors to happen" $ do
            err "foo" `shouldBe` "foo - no result"
            
        
