import Test.HUnit
import Data.Tiger

main :: IO ()
main =do 
    runTestTT $ TestList
        [
            executeExpTest
        ]
    return ()

executeExpTest :: Test
executeExpTest = TestList 
    [
        "executeExp test 1" ~: executeExp (NumExp 4) ~?= 4 ,
        "executeExp test 2" ~: 
            executeExp (OpExp (NumExp 4) Plus (NumExp 8)) ~?= 12 ,
        "executeExp test 3" ~: 
            executeExp ( EseqExp (PrintStm [(NumExp 114)]) (NumExp 7) ) ~?= 7
    ]
