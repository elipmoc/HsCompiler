module Parser where

import           Interpreter
import           Text.Parsec.Error  (ParseError)
import           Text.Parsec.Prim   (parse)
import           Text.Parsec.String (Parser)

--パーサの簡易実行と出力をする関数
run::Show a => Parser a -> String -> IO ()
run p input =
    case parse p "hoge" input of
        Left err ->
            putStr "parse error at" >> print err
        Right x  -> print x

--パースを実行する
parseStm :: String -> Either ParseError Stm
parseStm = undefined

