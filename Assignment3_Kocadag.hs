-- Faith-Anne Kocadag
-- CSCI 3330: Assignment 3: A Functional Interpreter
-- Questions 1, 2, and 3
-- April 18, 2013

type Ident = String				-- a name or identifier in BIL

-- Program is a data type, i.e. a collection of values
-- Prog is a constructor function that returns a Program
data Program = Prog [Decl] [Stmt]
	deriving (Show)

data Decl = Variable Ident
	deriving (Show)

data Stmt = Assign Ident Expr
	deriving (Show)

data Expr = Plus Expr Expr
	| Times Expr Expr
	| Inc Ident
	| Var Ident
	| Val Int
	deriving (Show)

-- Question #1: Use the starter file to define a constant called smallBil that represents the program

-- x;
declaration1 = Variable "x"

-- y;
declaration2 = Variable "y"

-- collect all declarations into a list
declarationList = [declaration1, declaration2]

-- x = 1
statement1 = Assign "x" (Val 1)

-- x++
statement2 = Assign "x" (Inc "x")

-- y = x + 4
statement3 = Assign "y" (Plus (Var "x") (Val 4))

-- collect all statements into a list.
statementList = [statement1, statement2, statement3]

-- create a smallBil to represent the program
smallBil = Prog declarationList statementList

-- Question #2: Define the abstract syntax below as Haskell data types. Build and print out in some fashion test programs. Comment the script with the concrete syntax of the programs.

-- Test Program 1
-- a;
declarationA = Variable "a"

-- b;
declarationB = Variable "b"

-- collect all declarations into a list.
declarationList2 = [declarationA, declarationB]

-- a = 1
statementA = Assign "a" (Val 1)

-- b = 2
statementB = Assign "b" (Val 2)

-- a = a + b
statementC = Assign "a" (Plus (Var "a") (Var "b"))

-- a++
statementD = Assign "a" (Inc "a")

-- collect all statements into a list
statementList2 = [statementA, statementB, statementC, statementD]

-- create myProgram to represent the program
myProgram = Prog declarationList2 statementList2

-- Test Program 2
-- e;
declarationE = Variable "e"

-- collect all declarations into a list.
declarationList3 = [declarationE]

-- e = 0
statementE = Assign "e" (Val 0)

-- e++
statementF = Assign "e" (Inc "e")

-- e = e * 7
statementG = Assign "e" (Times (Var "e") (Val 7))

-- collect all statements into a list
statementList3 = [statementE, statementF, statementG]

-- create myProgram to represent the program
myProgram2 = Prog declarationList3 statementList3

-- Question #3: We will not be concerned with concrete syntax in this project, other than as documentation. Rather, the interpreter will execute an abstract syntax structure, returning a "state" of variables mapped to integers. Implement such an interpreter. 

type State = [(Ident, Int)]		-- a list of identifier, value bindings (dynamic)

-- Set a value for a variable
setVal :: Ident -> Int -> State -> State
setVal ident value []		= []
setVal ident value (x:xs)	| (ident /= (fst x))	= x : setVal ident value xs
							| (ident == (fst x))	= ((fst x), value) : setVal ident value xs

-- Evaluate a statement
evalStmt :: Stmt -> State -> State
evalStmt (Assign ident expr) state 	= setVal ident (eval expr state) state

-- Run statements sequentially
runStmts :: [Stmt] -> State -> State
runStmts [] state		= state
runStmts (x:xs) state	= runStmts xs (evalStmt x state)
--runStmts (x:xs) state	= runStmts xs (setVal ident (eval expr state) state)

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