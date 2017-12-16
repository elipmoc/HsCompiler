module Main where

import           Interpreter


main :: IO ()
main =
    runStm prog
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

