import Test.Hspec
import Calc hiding (main)

main = hspec $ do
    describe "calc" $ do
        it "evaluates a single digit number" $ do
            calc "7" `shouldBe` Right 7
            calc "5" `shouldBe` Right 5

        it "allows unary operations" $ do
            calc "7~" `shouldBe` Right (-7)
            calc "7!" `shouldBe` Right 5040

        it "allows sequences of operations" $ do
            calc "7!~" `shouldBe` Right (-5040)

        it "evaluates any number" $ do
            calc "4807" `shouldBe` Right 4807

        it "allows for binary operations" $ do
            calc "3 4+" `shouldBe` Right 7
            calc "3 4*" `shouldBe` Right 12
            calc "3 4-" `shouldBe` Right (-1)
            calc "9 2/" `shouldBe` Right 4
            calc "9 2%" `shouldBe` Right 1

        it "shows message when a mistake is made" $ do
            calc "§" `shouldBe` Left "§ ??"
            calc "§ 3 4+" `shouldBe` Left "§ ??"

        it "checks for arguments on the stacks" $ do
            calc "!" `shouldBe` Left "not enough parameters"
            calc "3 +" `shouldBe` Left "not enough parameters"
            calc "/" `shouldBe` Left "not enough parameters"

        it "checks for division by zero" $ do
            calc "7 0/" `shouldBe` Left "division by zero"
            calc "7 0%" `shouldBe` Left "division by zero"

    describe "rpn" $ do
        it "outputs the calculations" $ do
            rpn [] `shouldBe` []
            rpn ["7 4+"] `shouldBe` ["11"]
            rpn ["7 0%"] `shouldBe` ["division by zero"]
            rpn ["7","3+"] `shouldBe` ["7","10"]

    
    
        
        



