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
	
main = do 
	contents <- getContents
	case parseArith contents of
		Left e -> do print e 
		Right term -> do putStrLn $ show $ eval term

isNumerical :: Term -> Bool
isNumerical term = case term of
	TmZero -> True
	TmSucc term -> isNumerical term
	TmPred term -> isNumerical term
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
	TmIsZero term -> case eval term of
		TmZero -> TmTrue
		t2 | isNumerical t2 -> TmFalse
		_ -> TmError
	TmPred TmZero -> TmZero
	TmPred (TmSucc term) -> eval term
	TmSucc term -> case eval term of
		t2 | isNumerical t2 -> TmSucc t2
		_ -> TmError
	_ -> TmError

valueParser :: String -> Term -> GenParser Char st Term
valueParser value term = string value >> return term

trueParser = valueParser "true" TmTrue

falseParser = valueParser "false" TmFalse

zeroParser = valueParser "0" TmZero

functionParser :: String -> (Term -> Term) -> GenParser Char st Term
functionParser name funcTerm = do
	string $ name ++ "(" 
	term <- arithParser
	char ')'
	return $ funcTerm term

succParser = functionParser "succ" TmSucc
predParser = functionParser "pred" TmPred
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

arithParser :: GenParser Char st Term
arithParser = try( ifParser ) 
		  <|> succParser 
		  <|> predParser 
		  <|> try ( isZeroParser ) 
		  <|> trueParser 
		  <|> falseParser 
		  <|> zeroParser

parseArith :: String -> Either ParseError Term
parseArith input = parse arithParser "Failed to parse arithmetic expression" input








