------------------------- Basics -------------------------
type VariableName = String
type Number = Int
type State = VariableName -> Int
------------------------- While language grammar -------------------------
data ArithmeticExp =
    NumberLiteral Number
    | Variable VariableName
    | Addition ArithmeticExp ArithmeticExp
    | Multiplication ArithmeticExp ArithmeticExp
    | Subtraction ArithmeticExp ArithmeticExp
    deriving (Show, Eq)

data BooleanExp =
    TrueLiteral
    | FalseLiteral
    | EqualTest ArithmeticExp ArithmeticExp
    | SmallerOrEqualTest ArithmeticExp ArithmeticExp
    | Negation BooleanExp
    | And BooleanExp BooleanExp
    deriving (Show, Eq)

data Statement =
    Assignment VariableName ArithmeticExp
    | Skip
    | Sequence Statement Statement
    | IfThenElse BooleanExp Statement Statement
    | WhileDo BooleanExp Statement
    deriving (Show, Eq)

------------------------- While Semantics -------------------------
arithmeticSemantic :: ArithmeticExp -> State -> Int
arithmeticSemantic (NumberLiteral n) state = n
arithmeticSemantic (Variable var) state = state var
arithmeticSemantic (Addition a1 a2) state = 
    arithmeticSemantic a1 state + arithmeticSemantic a2 state
arithmeticSemantic (Multiplication a1 a2) state = 
    arithmeticSemantic a1 state * arithmeticSemantic a2 state
arithmeticSemantic (Subtraction a1 a2) state = 
    arithmeticSemantic a1 state - arithmeticSemantic a2 state

booleanSemantic :: BooleanExp -> State -> Bool
booleanSemantic TrueLiteral state = True
booleanSemantic FalseLiteral state = False
booleanSemantic (EqualTest a1 a2) state = 
    arithmeticSemantic a1 state == arithmeticSemantic a2 state
booleanSemantic (SmallerOrEqualTest a1 a2) state = 
    arithmeticSemantic a1 state <= arithmeticSemantic a2 state
booleanSemantic (Negation b) state = not (booleanSemantic b state)
booleanSemantic (And b1 b2) state = 
    booleanSemantic b1 state && booleanSemantic b2 state

------------------------- Substitution Operators -------------------------
substitutionArithmetic :: ArithmeticExp -> VariableName -> ArithmeticExp -> ArithmeticExp
substitutionArithmetic (NumberLiteral n) var a_0 = NumberLiteral n
substitutionArithmetic (Variable existing_var) var a_0
    | existing_var == var = a_0
    | otherwise = Variable existing_var
substitutionArithmetic (Addition a_1 a_2) var a_0 = 
    Addition (substitutionArithmetic a_1 var a_0) (substitutionArithmetic a_2 var a_0)
substitutionArithmetic (Multiplication a_1 a_2) var a_0 = 
    Multiplication (substitutionArithmetic a_1 var a_0) (substitutionArithmetic a_2 var a_0)
substitutionArithmetic (Subtraction a_1 a_2) var a_0 = 
    Subtraction (substitutionArithmetic a_1 var a_0) (substitutionArithmetic a_2 var a_0)

substitutionBoolean :: BooleanExp -> VariableName -> ArithmeticExp -> BooleanExp
substitutionBoolean TrueLiteral var a_0 = TrueLiteral
substitutionBoolean FalseLiteral var a_0 = FalseLiteral
substitutionBoolean (EqualTest a_1 a_2) var a_0 = 
    EqualTest (substitutionArithmetic a_1 var a_0) (substitutionArithmetic a_2 var a_0)
substitutionBoolean (SmallerOrEqualTest a_1 a_2) var a_0 = 
    SmallerOrEqualTest (substitutionArithmetic a_1 var a_0) (substitutionArithmetic a_2 var a_0)
substitutionBoolean (Negation b_1) var a_0 = Negation (substitutionBoolean b_1 var a_0)
substitutionBoolean (And b_1 b_2) var a_0 = 
    And (substitutionBoolean b_1 var a_0) (substitutionBoolean b_2 var a_0)

substitutionState :: State -> VariableName -> Int -> State
substitutionState state var n param_var
    | var == param_var = n 
    | otherwise = state param_var

------------------------- Test Examples -------------------------
-- (-13) + (5 * 8) - x
a :: ArithmeticExp
a = Subtraction
    (Addition
        (NumberLiteral (-13))
        (Multiplication (NumberLiteral 5) (NumberLiteral 8))
    )
    (Variable "x")

-- (y + 4) <= (9 - y)
b :: BooleanExp
b = SmallerOrEqualTest 
    (Addition (Variable "y") (NumberLiteral 4))
    (Subtraction (NumberLiteral 9) (Variable "y"))

-- "compile" the example expressions
aCompiled :: State -> Int
aCompiled = arithmeticSemantic a

bCompiled :: State -> Bool
bCompiled = booleanSemantic b

-- test state with all variables set to 1
oneState :: State
oneState _ = 1

------------------------- Main -------------------------
main :: IO()
main = do
    -- "run/evaluate" compiled expressions
    print (aCompiled oneState) -- should be (-13) + (5 * 8) - 1 = 26 
    print (bCompiled oneState) --should be 5 <= 8 = True
