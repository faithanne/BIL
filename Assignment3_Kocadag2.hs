-- Faith-Anne Kocadag
-- CSCI 3330: Assignment 3: A Functional Interpreter
-- Question 4
-- April 18, 2013

type Ident = String				-- a name or identifier in BIL

-- Program is a data type, i.e. a collection of values
-- Prog is a constructor function that returns a Program
data Program = Prog [Decl] [Stmt]
	deriving (Show)

data Decl = Variable Ident
	deriving (Show)

data Stmt = Assign Ident Expr
	| While BoolExpr [Stmt]
	deriving (Show)

data BoolExpr = Equals Expr Expr
	| NotEquals Expr Expr
	deriving (Show)

data Expr = Plus Expr Expr
	| Times Expr Expr
	| Inc Ident
	| Var Ident
	| Val Int
	deriving (Show)

type State = [(Ident, Int)]		-- a list of identifier, value bindings (dynamic)

-- Set a value for a variable
setVal :: Ident -> Int -> State -> State
setVal ident value []		= []
setVal ident value (x:xs)	| (ident /= (fst x))	= x : setVal ident value xs
							| (ident == (fst x))	= ((fst x), value) : setVal ident value xs

-- Evalute a boolean expression:
boolEval :: BoolExpr -> State -> Bool
boolEval (Equals var1 var2) state		= (eval var1 state) == (eval var2 state)							
boolEval (NotEquals var1 var2) state	= (eval var1 state) /= (eval var2 state)
							
-- Evaluate a statement
evalStmt :: Stmt -> State -> State
evalStmt (Assign ident expr) state 	= setVal ident (eval expr state) state
evalStmt (While boolEx stmts) state	| ((boolEval boolEx state) == True)		= while boolEx stmts state
									| ((boolEval boolEx state) == False)	= state
										
-- Run statements sequentially
runStmts :: [Stmt] -> State -> State
runStmts [] state		= state
runStmts (x:xs) state	= runStmts xs (evalStmt x state)

-- while
while :: BoolExpr -> [Stmt] -> State -> State
while boolEx [] state		= state
while boolEx stmts state 	| ((boolEval boolEx state) == True)		= while boolEx stmts (runStmts stmts state)
							| ((boolEval boolEx state) == False)	= state							

-- Set declarations
inits :: [Decl] -> State -> State
inits [] state = state
inits xs state = ((getIdent (head xs)),0) : state  ++ inits (tail xs) state

-- Executing the program returns a final state
exec :: Program -> State
exec (Prog decls stmts) = runStmts stmts (inits decls [])

-- Return the integer binding of a variable
getVal :: Ident -> State -> Int
getVal ident []		= -1
getVal ident (x:xs)	| (ident == (fst x))	= snd x
					| otherwise				= getVal ident xs

-- Evaluate an expression
eval :: Expr -> State -> Int
eval (Plus var1 var2) state		= (eval var1 state) + (eval var2 state)
eval (Times var1 var2) state	= (eval var1 state) * (eval var2 state)
eval (Inc ident) state			= (getVal ident state) + 1
eval (Var ident) state			= (getVal ident state)
eval (Val int) state			= int

-- Get Ident from Decl
getIdent :: Decl -> Ident
getIdent (Variable a) = a

-- Create a state
createState :: Ident -> Int -> State
createState ident value = [(ident, value)]

-- Add a declaration to a state
elab :: Decl -> State -> State
elab decl (x:xs)	= createState (getIdent decl) (snd x)

-- Test Program #3
-- h;
declarationH = Variable "h"

-- collect all declarations into a list
declarationList4 = [declarationH]

-- h = 0
statementH = Assign "h" (Val 0)

-- while (h != 5), h++
statementI = While (NotEquals (Var "h") (Val 5)) [Assign "h" (Inc "h")]

-- collect all statements into a list
statementList4 = [statementH, statementI]

-- create myProgram to represent the program
whileProgram = Prog declarationList4 statementList4