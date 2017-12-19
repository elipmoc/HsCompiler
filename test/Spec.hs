import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           Interpreter
import           Parser
import           Test.HUnit

main :: IO ()
main =do
    runTestTT $ TestList
        [
            executeExpTest
            ,parseStmTest
        ]
    return ()

executeExpTestHelper :: Exp->Int
executeExpTestHelper e =
    case evalState (executeExp e) (Map.singleton "a" 45 ::VariableMap) of
        (_,int) -> int

executeExpTest :: Test
executeExpTest = TestList
    [
        "executeExp test 1" ~: executeExpTestHelper (NumExp 4) ~?= 4 ,
        "executeExp test 2" ~:
            executeExpTestHelper (OpExp (NumExp 4) Plus (NumExp 8)) ~?= 12 ,
        "executeExp test 3" ~:
            executeExpTestHelper (OpExp (NumExp 4) Minus (NumExp 8)) ~?= -4 ,
        "executeExp test 4" ~:
            executeExpTestHelper (OpExp (NumExp 4) Times (NumExp 8)) ~?= 32 ,
        "executeExp test 5" ~:
            executeExpTestHelper (OpExp (NumExp 16) Div (NumExp 8)) ~?= 2 ,
        "executeExp test 6" ~:
            executeExpTestHelper ( EseqExp (PrintStm [NumExp 114]) (NumExp 7) ) ~?= 7 ,
        "executeExp test 7" ~:
            executeExpTestHelper (IdExp "a")  ~?= 45
    ]

unParseStm :: String -> Stm
unParseStm s = case parseStm s of
    Right x -> x
    Left x  -> error $ show x

parseStmTest :: Test
parseStmTest =TestList
    [
        "parseStm test 1" ~: unParseStm "a:=5;" ~?= AssignStm "a" (NumExp 5)
        ,"parseStm test 2" ~: unParseStm "b:=4+5*6/2-1;" ~?=
            AssignStm "b"
                (
                    OpExp
                    (
                        OpExp
                            (NumExp 4)
                            Plus
                            (
                                OpExp
                                    (
                                        OpExp
                                            (NumExp 5)
                                            Times
                                            (NumExp 6)
                                    )
                                    Div
                                    (NumExp 2)
                            )
                    )
                    Minus
                    (NumExp 1)
                )
        ,"parseStm test 3" ~: unParseStm "print(5+1);" ~?=
            PrintStm [OpExp (NumExp 5) Plus (NumExp 1)]
        ,"parseStm test 4" ~: unParseStm "print(5,4,3);" ~?=
            PrintStm [NumExp 5 ,NumExp 4 ,NumExp 3 ]
        ,"parseStm test 5" ~: unParseStm "a:=79;print(5);" ~?=
            CompoundStm (AssignStm "a" (NumExp 79)) (PrintStm [NumExp 5])
        ,"parseStm test 6" ~: unParseStm "a:=5;b:=2;print(a+b);" ~?=
            CompoundStm
                (AssignStm "a" (NumExp 5))
                (
                    CompoundStm
                        (AssignStm "b" (NumExp 2))
                        (PrintStm [OpExp (IdExp "a") Plus (IdExp "b")])
                )
    ]
