data Term =
	TmTrue 				|
	TmFalse 			|
	TmIf Term Term Term |
	TmZero 				|
	TmSucc Term 		|
	TmPred Term 		|
	TmIsZero Term 		|
	TmError
	deriving Show
	
main = do 
	putStr $ show $ eval $ TmIf (TmIsZero (TmSucc TmZero)) (TmSucc TmZero) TmZero

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