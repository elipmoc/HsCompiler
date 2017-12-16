module  Interpreter where

import           Control.Monad.State
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import qualified Text.Parsec         as P


type Id =String

type VariableMap = Map.Map Id Int

type PrintStr = [String]

data Binop = Plus | Minus | Times | Div deriving (Show,Eq)

--抽象構文のためのデータ構造
data Stm =
    CompoundStm Stm Stm
    | AssignStm Id Exp
    | PrintStm [Exp]
    deriving (Show,Eq)

data Exp =
    IdExp Id
    | NumExp Int
    | OpExp Exp Binop Exp
    | EseqExp Stm Exp
    deriving (Show,Eq)

--二項演算子の結びつけ
binopFunc::Binop->(Int->Int->Int)
binopFunc Plus  = (+)
binopFunc Minus = (-)
binopFunc Times = (*)
binopFunc Div   = div

--式を実行して結果を得る
executeExp :: Exp -> State VariableMap (PrintStr,Int)
executeExp (NumExp i) = return ([],i)
executeExp (OpExp e1 binop e2) = do
    (strList1,v1) <- executeExp e1
    (strList2,v2) <- executeExp e2
    return (strList1++strList2,binopFunc binop v1 v2)
executeExp (IdExp id) = do
    vmap <- get
    return ([],fromJust $ Map.lookup id vmap)
executeExp (EseqExp stm e) = do
    strList1 <- executeStm stm
    (strList2,int) <- executeExp e
    return (strList1++strList2,int)

--文の実行をする
executeStm :: Stm -> State VariableMap PrintStr
executeStm (CompoundStm stm1 stm2) = do
    strList1 <- executeStm stm1
    strList2 <- executeStm stm2
    return $ strList1 ++ strList2
executeStm (AssignStm id exp) = do
    vmap <- get
    (strList,v) <- executeExp exp
    put $ Map.insert id v vmap
    return strList
executeStm (PrintStm exps) =
    foldM
        (
            \a b -> do
                (strList,int) <- executeExp b
                return $ a++strList++[show int]
        ) [] exps

--文をガチで実行する
runStm::Stm->IO()
runStm stm = forM_ (evalState (executeStm stm) Map.empty) print
