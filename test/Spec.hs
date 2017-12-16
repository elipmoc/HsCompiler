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
        "parseStm test 1" ~: unParseStm "a=5" ~?= AssignStm "a" (NumExp 5)
        ,"parseStm test 2" ~: unParseStm "b=4+5*6/2" ~?=
            AssignStm "b" (OpExp (NumExp 4) Plus (OpExp (NumExp 5) Times (OpExp (NumExp 6) Div (NumExp 2))))
    ]
