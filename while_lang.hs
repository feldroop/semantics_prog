------------------------- Some Variable set -------------------------
-- Could probably also be a String
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

------------------------- Test Examples -------------------------
-- -1101_base2 = -13_base10
aNumber :: Number
aNumber = Negative (Concat (Concat (Concat (Highest One) One) Zero) One)

-- 101_base2 = 5_base10
anotherNumber :: Number
anotherNumber = Positive (Concat (Concat (Highest One) Zero) One)

------------------------- Main -------------------------
main :: IO()
main = do
    print (numberSemantics aNumber) -- should be -13
    print (numberSemantics anotherNumber) -- should be 5
