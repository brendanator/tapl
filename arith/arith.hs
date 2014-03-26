import Control.Monad (forM_)
import Control.Applicative hiding ((<|>))
import System.Environment (getArgs)
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
	
main :: IO()
main = do 
	args <- getArgs
	forM_ args (\arg -> case parseArith arg of
				Left err -> print err 
				Right term -> print $ eval term)

isNumerical :: Term -> Bool
isNumerical term = case term of
	TmZero -> True
	TmSucc subterm -> isNumerical subterm
	TmPred subterm -> isNumerical subterm
	_ -> False

eval :: Term -> Term
eval TmTrue = TmTrue
eval TmFalse = TmFalse
eval TmZero = TmZero
eval (TmIf term1 term2 term3) = evalIf (eval term1) term2 term3
eval (TmIsZero subterm) = evalIsZero $ eval subterm
eval (TmPred subterm) = evalPred $ eval subterm
eval (TmSucc subterm) = evalSucc $ eval subterm
eval _ = TmError

evalIf :: Term -> Term -> Term -> Term
evalIf TmTrue a _ = eval a
evalIf TmFalse _ b = eval b
evalIf _ _ _ = TmError

evalIsZero :: Term -> Term
evalIsZero TmZero = TmTrue
evalIsZero term 
	| isNumerical term = TmFalse
	| otherwise        = TmError

evalPred :: Term -> Term
evalPred TmZero = TmZero
evalPred (TmSucc subterm) = eval subterm
evalPred _ = TmError

evalSucc :: Term -> Term
evalSucc term
	| isNumerical term = TmSucc term
	| otherwise        = TmError

parseArith :: String -> Either ParseError Term
parseArith input = parse arithParser "Failed to parse arithmetic expression" input

arithParser :: GenParser Char st Term
arithParser = try( ifParser ) 
		  <|> try( succParser )
		  <|> try( predParser )
		  <|> try( isZeroParser ) 
		  <|> try( trueParser )
		  <|> try( falseParser )
		  <|> zeroParser

trueParser :: GenParser Char st Term
trueParser = string "true" >> return TmTrue

falseParser :: GenParser Char st Term
falseParser = string "false" >> return TmFalse

zeroParser :: GenParser Char st Term
zeroParser = char '0' >> return TmZero

functionParser :: String -> (Term -> Term) -> GenParser Char st Term
functionParser name funcTerm = 
	funcTerm <$> (string name *> char '(' *> spaces *> arithParser <* spaces <* char ')')

succParser :: GenParser Char st Term
succParser = functionParser "succ" TmSucc

predParser :: GenParser Char st Term
predParser = functionParser "pred" TmPred

isZeroParser :: GenParser Char st Term
isZeroParser = functionParser "iszero" TmIsZero

ifParser :: GenParser Char st Term
ifParser = 
	TmIf <$> (string "if" *> spaces *> arithParser) 
		 <*> (spaces *> string "then" *> spaces *> arithParser)
		 <*> (spaces *> string "else" *> spaces *> arithParser)
