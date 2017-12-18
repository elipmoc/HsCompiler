module Parser where

import           Control.Applicative    ((*>), (<$>), (<*), (<*>), (<|>))
import           Control.Monad
import           Control.Monad.Identity
import           Interpreter
import           Text.Parsec.Char       (char, digit, letter, string)
import           Text.Parsec.Combinator (eof, many1, sepBy)
import           Text.Parsec.Error      (ParseError)
import qualified Text.Parsec.Expr       as E
import           Text.Parsec.Prim       (parse, try)
import           Text.Parsec.String     (Parser)

--パースを実行する
parseStm :: String -> Either ParseError Stm
parseStm = parse parserStm ""

parserStm :: Parser Stm
parserStm =
    try (parserStm2 <* eof) <|> parserCompoundStm

parserStm2 :: Parser Stm
parserStm2 = (try parserAssignStm <|> parserPrintStm) <* char ';'

parserCompoundStm :: Parser Stm
parserCompoundStm = CompoundStm <$> parserStm2 <*> parserStm

parserAssignStm :: Parser Stm
parserAssignStm = AssignStm <$> parserId <* string ":=" <*> parserExpr

parserPrintStm :: Parser Stm
parserPrintStm = do
    string "print"
    char '('
    PrintStm <$> parserArgsExp <* char ')'

parserArgsExp :: Parser [Exp]
parserArgsExp = parserExpr `sepBy` (char ',')

parserId :: Parser Id
parserId =  many1 letter

parserTermExp :: Parser Exp
parserTermExp =
    IdExp <$> parserId
    <|> parserNumExp

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
        E.Infix $ mkBinOp <$> parserBinop name
    mkBinOp op a b = OpExp a op b

parserBinop :: String -> Parser Binop
parserBinop "+"=char '+' >> return Plus
parserBinop "-"=char '-' >> return Minus
parserBinop "*"=char '*' >> return Times
parserBinop "/"=char '/' >> return Div

parserExpr :: Parser Exp
parserExpr = E.buildExpressionParser opTable parserTermExp

