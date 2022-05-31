------------------------- Basics -------------------------
type VariableName = String
type Number = Integer
type State = VariableName -> Number
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
arithmeticSemantic :: ArithmeticExp -> State -> Number
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

statementSemantic :: Statement -> State -> State
-- param_var is the result(Variable -> Int)'s input (Variable)
statementSemantic Skip s param_var = s param_var
statementSemantic stm@(Assignment var a) s param_var
    | var == param_var = arithmeticSemantic a s
    | otherwise = s param_var
statementSemantic (Sequence stm1 stm2) s param_var = statementSemantic stm2 (statementSemantic stm1 s) param_var
statementSemantic (IfThenElse b stm1 stm2) s param_var
    | booleanSemantic b s = statementSemantic stm1 s param_var
    | otherwise = statementSemantic stm2 s param_var
statementSemantic while@(WhileDo b stm) s param_var
    | booleanSemantic b s = statementSemantic while (statementSemantic stm s) param_var
    | otherwise = statementSemantic Skip s param_var

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

substitutionState :: State -> VariableName -> Number -> State
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

-- substitution examples
aSubstituted :: ArithmeticExp
aSubstituted = substitutionArithmetic a "x" (NumberLiteral 25)

-- (-13) + (5 * 8) - 25
aExpected :: ArithmeticExp
aExpected = Subtraction
    (Addition
        (NumberLiteral (-13))
        (Multiplication (NumberLiteral 5) (NumberLiteral 8))
    )
    (NumberLiteral 25)

bSubstituted :: BooleanExp
bSubstituted = substitutionBoolean b "y" (Multiplication (NumberLiteral 3) (Variable "x"))

-- ((3 * x) + 4) <= (9 - (3 * x))
bExpected :: BooleanExp
bExpected = SmallerOrEqualTest
    (Addition
        (Multiplication (NumberLiteral 3) (Variable "x"))
        (NumberLiteral 4)
    )
    (Subtraction
        (NumberLiteral 9)
        (Multiplication (NumberLiteral 3) (Variable "x"))
    )

factorialExample :: Statement
factorialExample =
    Sequence
        (Assignment "y" $ NumberLiteral 1)
        (
            WhileDo (Negation $ EqualTest (Variable "x") (NumberLiteral 1))
            (
                Sequence
                    (Assignment "y" $ Multiplication (Variable "x") (Variable "y"))
                    (Assignment "x" $ Subtraction (Variable "x") (NumberLiteral 1))
            )
        )

factorialExampleState :: State
factorialExampleState "x" = 6
factorialExampleState _ = 1

isEven :: Statement
isEven =
    Sequence
    (
        Assignment "y" $ NumberLiteral 0
    )
    (
        Sequence
        (
            WhileDo (SmallerOrEqualTest (Multiplication (Variable "y") (NumberLiteral 2)) (Variable "x"))
            (
                Assignment "y" (Addition (Variable "y") $ NumberLiteral 1)
            )
        )
        (
            IfThenElse (EqualTest (Variable "x") (Multiplication (Subtraction (Variable "y") (NumberLiteral 1)) $ NumberLiteral 2))
            (Assignment "y" $ NumberLiteral 1)
            (Assignment "y" $ NumberLiteral 0)
        )
    )

-- test state with all variables set to 1
oneState :: State
oneState _ = 1

isEqual :: Statement
isEqual =
    IfThenElse (EqualTest (Variable "x") (Variable "y"))
    (Assignment "z" $ NumberLiteral 1)
    (Assignment "z" $ NumberLiteral 0)


------------------------- Main -------------------------
main :: IO()
main = do
    -- "run/evaluate" compiled expressions
    print (arithmeticSemantic a oneState) -- should be (-13) + (5 * 8) - 1 = 26
    print (booleanSemantic b oneState) --should be 5 <= 8 = True

    -- test substitution operator
    print (arithmeticSemantic aSubstituted oneState) -- (-13) + (5 * 8) - 25 = 2
    print (aSubstituted == aExpected) -- should be True
    print (booleanSemantic bSubstituted oneState) -- 7 <= 6 = False
    print (bSubstituted == bExpected) -- should be True

    -- test lemma from exercise 2 (should both be True)
    print (
        arithmeticSemantic aSubstituted oneState
        == arithmeticSemantic a (substitutionState oneState "x" 25)
        )
    print (
        booleanSemantic bSubstituted oneState
        == booleanSemantic b (substitutionState oneState "y" 3)
        )
    -- test the factorial statement evaluation (as "x" = 6, this should equal fac(6)=720)
    print(statementSemantic factorialExample factorialExampleState "y" == 720)

