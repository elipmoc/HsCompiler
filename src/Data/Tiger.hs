module Data.Tiger where

import qualified Data.Map.Strict as Map

testMap :: Map.Map Char Int
testMap = Map.singleton 'v' 45

type Id =String

data Binop =Plus | Minus | Times | Div deriving Show

data Stm = 
    CompoundStm Stm Stm
    | AssignStm Id Exp
    | PrintStm [Exp] 
    deriving Show

data Exp =
    IdExp Id
    | NumExp Int
    | OpExp Exp Binop Exp
    | EseqExp Stm Exp 
    deriving Show

--式を実行して結果を得る
executeExp :: Exp -> Int
executeExp =undefined
