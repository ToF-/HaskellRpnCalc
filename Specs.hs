import Test.Hspec

main = hspec $ do
    describe "RPN calcuator" $ do
        it "should pass a dummy test" $ do
            1+1 `shouldBe` 2


