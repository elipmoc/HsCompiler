import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           Data.Tiger
import           Test.HUnit

main :: IO ()
main =do
    runTestTT $ TestList
        [
            executeExpTest
        ]
    return ()

executeExpTestHelper :: Exp->Int
executeExpTestHelper e = evalState (executeExp e) (Map.singleton "a" 7 ::VariableMap)

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
            executeExpTestHelper (IdExp "a")  ~?= 7

    ]
