module Parser where

import           Control.Applicative    ((*>), (<$>), (<*), (<*>), (<|>))
import           Control.Monad
import           Interpreter
import           Text.Parsec.Char       (char, digit, letter)
import           Text.Parsec.Combinator (many1)
import           Text.Parsec.Error      (ParseError)
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
parseStm input = parse parserAssignStm "" input

parserAssignStm :: Parser Stm
parserAssignStm = AssignStm <$> parserId <* char '=' <*> parserPlusMinusExp

parserNumExp ::Parser Exp
parserNumExp = do
    numStr <- many1 digit
    return $ NumExp (read numStr::Int)

parserPlusMinusExp :: Parser Exp
parserPlusMinusExp =
    try(do
        a <- parserTimesDivExp
        b <- parserPlusMinusBinop
        c <- parserPlusMinusExp
        return $ OpExp a b c)
    <|> parserTimesDivExp

parserPlusMinusBinop :: Parser Binop
parserPlusMinusBinop =
    (char '+' >> return Plus)
    <|> (char '-' >> return Minus)


parserTimesDivExp :: Parser Exp
parserTimesDivExp =
    try(do
        a <- parserNumExp
        b <- parserTimesDivBinop
        c <- parserTimesDivExp
        return $ OpExp a b c)
    <|> parserNumExp

parserTimesDivBinop :: Parser Binop
parserTimesDivBinop =
    (char '*' >> return Times)
    <|> (char '/' >> return Div)

parserId :: Parser Id
parserId =  many1 letter

