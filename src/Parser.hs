module Parser where

import           Control.Applicative    ((*>), (<$>), (<*), (<*>), (<|>))
import           Control.Monad
import           Control.Monad.Identity
import           Interpreter
import           Text.Parsec.Char       (char, digit, letter, string)
import           Text.Parsec.Combinator (many1)
import           Text.Parsec.Error      (ParseError)
import qualified Text.Parsec.Expr       as E
import           Text.Parsec.Prim       (parse, try)
import           Text.Parsec.String     (Parser)

--パーサの簡易実行と出力をする関数
run::Show a => Parser a -> String -> IO ()
run p input =
    case parse p "hoge" input of
        Left err ->
            putStr "parse error at" >> print err
        Right x  -> print x

--パースを実行する
parseStm :: String -> Either ParseError Stm
parseStm = parse parserAssignStm ""

parserAssignStm :: Parser Stm
parserAssignStm = AssignStm <$> parserId <* string ":=" <*> parserExpr

parserId :: Parser Id
parserId =  many1 letter

parserNumExp :: Parser Exp
parserNumExp = do
    numStr <- many1 digit
    return $ NumExp (read numStr::Int)

opTable :: [[E.Operator String () Identity Exp]]
opTable =
    [
        [
            binary "*" E.AssocLeft
            ,binary "/" E.AssocLeft
         ]
        ,[
            binary "+" E.AssocLeft
            ,binary "-" E.AssocLeft
         ]
    ]
  where
    binary name =
        E.Infix
            (
                mkBinOp <$> parserBinop name
            )
    mkBinOp op a b = OpExp a op b

parserBinop :: String -> Parser Binop
parserBinop "+"=char '+' >> return Plus
parserBinop "-"=char '-' >> return Minus
parserBinop "*"=char '*' >> return Times
parserBinop "/"=char '/' >> return Div

parserExpr :: Parser Exp
parserExpr = E.buildExpressionParser opTable parserNumExp

