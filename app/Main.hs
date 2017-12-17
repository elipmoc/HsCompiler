module Main where

import           Interpreter
import           Parser
import           System.Environment


main :: IO ()
main =do
    (s:_) <- getArgs
    case parseStm s of
        Left x  -> print x
        Right x -> runStm x
--    runStm prog
    where prog =
            CompoundStm
            (
                AssignStm
                    "a"
                    (OpExp (NumExp 5) Plus (NumExp 3))
            )
            (
                CompoundStm
                    (
                        AssignStm
                            "b"
                            (
                                EseqExp
                                    (PrintStm [IdExp "a",OpExp (IdExp "a") Minus (NumExp 1)])
                                    (OpExp (NumExp 10) Times (IdExp "a"))
                            )
                    )
                    ( PrintStm [IdExp "b"])
            )

