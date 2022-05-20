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
    deriving Show

data BooleanExp =
    TrueLiteral
    | FalseLiteral
    | EqualTest ArithmeticExp ArithmeticExp
    | SmallerOrEqualTest ArithmeticExp ArithmeticExp
    | Negation BooleanExp
    | And BooleanExp BooleanExp
    deriving Show

data Statement =
    Assignment VariableName ArithmeticExp
    | Skip
    | Sequence Statement Statement
    | IfThenElse BooleanExp Statement Statement
    | WhileDo BooleanExp Statement
    deriving Show

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

------------------------- Test Examples -------------------------
-- (-13) + (5 * 5) - X
anArithmeticExpression :: ArithmeticExp
anArithmeticExpression =
    Subtraction
        (Addition
            (NumberLiteral (-13))
            (Multiplication 
                (NumberLiteral 5)
                (NumberLiteral 5)
            )
        )
        (Variable "x")

-- (anArithmeticExpression == Y) AND not false
aBooleanExpression :: BooleanExp
aBooleanExpression = 
    And 
        (EqualTest
            anArithmeticExpression
            (Variable "y")
        )
        (Negation FalseLiteral)

-- "compile" the example expressions
aCompiledArithmeticExp :: State -> Int
aCompiledArithmeticExp = arithmeticSemantic anArithmeticExpression

aCompiledBooleanExp :: State -> Bool
aCompiledBooleanExp = booleanSemantic aBooleanExpression

-- test state with all variables set to 0
nullState :: State
nullState _ = 0

------------------------- Main -------------------------
main :: IO()
main = do
    -- "run/evaluate" compiled expressions
    print (aCompiledArithmeticExp nullState) -- should be (-13) + (5 * 5) - 0 = 12 
    print (aCompiledBooleanExp nullState) --should be (12 == 0) AND not false = False
