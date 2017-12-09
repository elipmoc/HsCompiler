module Data.Tiger where

import qualified Data.Map.Strict as Map
import Control.Monad.State

testMap :: Map.Map Char Int
testMap = Map.singleton 'v' 45

type Id =String

type VariableMap = Map.Map Id Int

emptyVariableMap :: VariableMap
emptyVariableMap = Map.empty::VariableMap

data Binop = Plus | Minus | Times | Div deriving Show

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
executeExp :: Exp -> State VariableMap Int
executeExp (NumExp i) = return i
executeExp (OpExp e1 Plus e2) = do 
    v1 <- (executeExp e1) 
    v2 <- (executeExp e2)
    return $ v1 + v2  
executeExp (OpExp e1 Minus e2) = do 
    v1 <- (executeExp e1) 
    v2 <- (executeExp e2)
    return $ v1 - v2   
executeExp (OpExp e1 Times e2) = do 
    v1 <- (executeExp e1) 
    v2 <- (executeExp e2)
    return $ v1 * v2   
executeExp (OpExp e1 Div e2) = do 
    v1 <- (executeExp e1) 
    v2 <- (executeExp e2)
    return $ v1 `div` v2  
executeExp (EseqExp _ e) = executeExp e
