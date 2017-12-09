module Main where

import Data.Tiger
import Lib

prog =
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
    
main :: IO ()
main = do 
    print prog
    print testMap
