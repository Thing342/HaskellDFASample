import DFA

data DFAAlphabet = A | B deriving (Eq, Show)
data DFAState = Q1 | Q2 | Q3 | Q4 | Q5 deriving(Eq, Show)

instance Alphabet DFAAlphabet where
    decode ('a':rest) = Right (rest, A)
    decode ('b':rest) = Right (rest, B)
    decode other = Left ("Invalid letter: " ++ other)

t =[Q1 >== A ==> Q3, Q1 >== B ==> Q2,
    Q2 >== A ==> Q1, Q2 >== B ==> Q4,
    Q3 >== A ==> Q5, Q3 >== B ==> Q1,
    Q4 >== A ==> Q2, Q4 >== B ==> Q2,
    Q5 >== A ==> Q3, Q5 >== B ==> Q3]

mydfa :: DFA DFAState DFAAlphabet
mydfa = DFA (delta t) Q1 [Q2]