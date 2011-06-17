module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import System

data DateVal = DayOfMonth Int
			 | DayOfWeek Int
             | NumericMonth Int
             | WrittenMonth Int
             | Year Int
             | Hours Int
             | Minutes Int
instance Show DateVal 
	where 
		show = showVal

spaces :: Parser ()
spaces = skipMany1 space

pointSpaces :: Parser ()
pointSpaces = char "." >> skipMany space

parseNumericDay :: Parser DateVal
parseNumericDay = do
				day <- many1 digit
				pointSpaces
				let iDay = read day 
				return $ if (iDay >= 1) && (iDay <= 31)
                         then NumericDay iDay
						 else fail "no match for parseNumericDay"

parseNumericMonth :: Parser DateVal
parseNumericMonth = do
				month <- many1 digit
				pointSpaces
				let iMonth = read month
				return $ if (iMonth >= 1) && (iMonth <= 12)
                         then NumericMonth iMonth
						 else fail "no match for parseNumericMonth"

parseExpr :: Parser LispVal
parseExpr = parseNumericDay
        <|> parseNumericMonth

showVal :: LispVal -> String
showVal (Number contents) = show contents

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

