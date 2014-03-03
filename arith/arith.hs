import Control.Monad  
import System.Environment   
import Text.ParserCombinators.Parsec

data Term = TmTrue
		  | TmFalse
		  | TmIf Term Term Term
		  | TmZero
		  | TmSucc Term
		  | TmPred Term
		  | TmIsZero Term
		  | TmError
	deriving Show
	
main :: IO[()]
main = do 
	args <- getArgs
	forM args (\arg -> case parseArith arg of
				Left e -> print e 
				Right term -> print $ eval term)

isNumerical :: Term -> Bool
isNumerical term = case term of
	TmZero -> True
	TmSucc subterm -> isNumerical subterm
	TmPred subterm -> isNumerical subterm
	_ -> False

eval :: Term -> Term
eval term = case term of 
	TmTrue -> TmTrue
	TmFalse -> TmFalse
	TmZero -> TmZero
	TmIf term1 term2 term3 -> case eval term1 of
		TmTrue -> eval term2
		TmFalse -> eval term3
		_ -> TmError
	TmIsZero subterm -> case eval subterm of
		TmZero -> TmTrue
		t2 | isNumerical t2 -> TmFalse
		_ -> TmError
	TmPred TmZero -> TmZero
	TmPred (TmSucc subterm) -> eval subterm
	TmSucc subterm -> case eval subterm of
		t2 | isNumerical t2 -> TmSucc t2
		_ -> TmError
	_ -> TmError

parseArith :: String -> Either ParseError Term
parseArith input = parse arithParser "Failed to parse arithmetic expression" input

arithParser :: GenParser Char st Term
arithParser = try( ifParser ) 
		  <|> try( succParser )
		  <|> try( predParser )
		  <|> try( isZeroParser ) 
		  <|> try( trueParser )
		  <|> try( falseParser )
		  <|> try( zeroParser )

trueParser :: GenParser Char st Term
trueParser = string "true" >> return TmTrue

falseParser :: GenParser Char st Term
falseParser = string "false" >> return TmFalse

zeroParser :: GenParser Char st Term
zeroParser = char '0' >> return TmZero

functionParser :: String -> (Term -> Term) -> GenParser Char st Term
functionParser name funcTerm = do
	string $ name ++ "(" 
	term <- arithParser
	char ')'
	return $ funcTerm term

succParser :: GenParser Char st Term
succParser = functionParser "succ" TmSucc

predParser :: GenParser Char st Term
predParser = functionParser "pred" TmPred

isZeroParser :: GenParser Char st Term
isZeroParser = functionParser "iszero" TmIsZero

ifParser :: GenParser Char st Term
ifParser = do
	string "if"
	spaces
	term1 <- arithParser
	spaces
	string "then"
	spaces
	term2 <- arithParser
	spaces
	string "else"
	spaces
	term3 <- arithParser
	return $ TmIf term1 term2 term3
