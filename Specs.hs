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
                let [(Const n,_)] = parse "17"
                n  `shouldBe` 17

            it "parses a string into an unary operator" $ do
                let [(Unary f,_)] = parse "!"
                f 4 `shouldBe` 24

            it "parses a string into a binary operator" $ do
                let [(Binary f,_)] = parse "+"
                f 42 17 `shouldBe` 59

            it "yields the rest to parse as well as the token" $ do
                let [(Const n,s)] = parse "42!17+"
                s `shouldBe` "!17+"
                let [(Unary _,s)] = parse "!17+"
                s `shouldBe` "17+"
                let [(Binary _,s)] = parse "+2-"
                s `shouldBe` "2-"

            it "doesn't parse a minus prefix to a number" $ do
                let ts = parse "-5" 
                null ts  `shouldBe` True

        describe "parseRpn" $ do
            it "should parse a whole RPN expression" $ do
                let [Const n ,Unary f ,Const m ,Binary p] = parseRPN "4!17+" 
                p (f n) m `shouldBe` (4*3*2)+17

        describe "evalRpn" $ do
            it "should eval a whole list of tokens" $ do
                let ts = parseRPN "4!17+" 
                evalRPN ts `shouldBe` (4*3*2)+17

            it "should eval expressions with many different operators" $ do
                let e = evalRPN . parseRPN
                e "4 5-"  `shouldBe` (-1)
                e "40 5/" `shouldBe` 8
                e "42 5%" `shouldBe` 2
                e "2 5^"  `shouldBe` 32
                e "42~"   `shouldBe` (-42)
                e "5!"    `shouldBe` 120 
                e "5 7-3*" `shouldBe` (-6)
                let ts = parseRPN "4~3*5/4^7%"
                evalRPN ts `shouldBe` (((((-4)*3)`div`5)^4)`mod`7)
                let ts = parseRPN "4~3*5/4^7%2+5 7-2*+"
                evalRPN ts `shouldBe` (((((-4)*3)`div`5)^4)`mod`7)+((5-7)*2)
                

        

