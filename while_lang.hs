------------------------- Some Variable set -------------------------
-- Could also be a String
data VariableName = A | B | C | I | J | K | M | N | X | Y | Z
    deriving Show

------------------------- Number grammar -------------------------
data Bit = 
    Zero 
    | One
    deriving Show

data Bits = 
    Highest Bit
    | Concat Bits Bit
    deriving Show

data Number =
    Positive Bits
    | Negative Bits
    deriving Show

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

------------------------- Number Semantics -------------------------
numberSemantics :: Number -> Int
numberSemantics (Positive bs) = numberSemanticsHelp bs
numberSemantics (Negative bs) = -1 * numberSemanticsHelp bs

numberSemanticsHelp :: Bits -> Int
numberSemanticsHelp (Highest b) = bitValue b
numberSemanticsHelp (Concat bs b) = 2 * numberSemanticsHelp bs + bitValue b

bitValue :: Bit -> Int
bitValue Zero = 0
bitValue One = 1

------------------------- While Semantics -------------------------
arithmeticSemantic :: ArithmeticExp -> (VariableName -> Int) -> Int
arithmeticSemantic (NumberLiteral n) state = numberSemantics n
arithmeticSemantic (Variable var) state = state var
arithmeticSemantic (Addition a1 a2) state = 
    arithmeticSemantic a1 state + arithmeticSemantic a2 state
arithmeticSemantic (Multiplication a1 a2) state = 
    arithmeticSemantic a1 state * arithmeticSemantic a2 state
arithmeticSemantic (Subtraction a1 a2) state = 
    arithmeticSemantic a1 state - arithmeticSemantic a2 state

booleanSemantic :: BooleanExp -> (VariableName -> Int) -> Bool
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
-- -1101_base2 = -13_base10
aNumber :: Number
aNumber = Negative (Concat (Concat (Concat (Highest One) One) Zero) One)

-- 101_base2 = 5_base10
anotherNumber :: Number
anotherNumber = Positive (Concat (Concat (Highest One) Zero) One)

-- (-13) + (5 * 5) - X
anArithmeticExpression :: ArithmeticExp
anArithmeticExpression =
    Subtraction
        (Addition
            (NumberLiteral aNumber)
            (Multiplication 
                (NumberLiteral anotherNumber)
                (NumberLiteral anotherNumber)
            )
        )
        (Variable X)

-- (anArithmeticExpression == Y) AND not false
aBooleanExpression :: BooleanExp
aBooleanExpression = 
    And 
        (EqualTest
            anArithmeticExpression
            (Variable Y)
        )
        (Negation FalseLiteral)

-- "compile" the example expressions
aCompiledArithmeticExp :: (VariableName -> Int ) -> Int
aCompiledArithmeticExp = arithmeticSemantic anArithmeticExpression

aCompiledBooleanExp :: (VariableName -> Int ) -> Bool
aCompiledBooleanExp = booleanSemantic aBooleanExpression

-- test state with all variables set to 0
nullState :: VariableName -> Int 
nullState _ = 0

------------------------- Main -------------------------
main :: IO()
main = do
    print (numberSemantics aNumber) -- should be -13
    print (numberSemantics anotherNumber) -- should be 5

    -- "run/evaluate" compiled expressions
    print (aCompiledArithmeticExp nullState) -- should be (-13) + (5 * 5) - 0 = 12 
    print (aCompiledBooleanExp nullState) --should be (12 == 0) AND not false = False
