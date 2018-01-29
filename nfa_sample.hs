import NFA

data MyAlphabet = A | B deriving (Eq, Show)
data MyState = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 deriving(Eq, Show)

instance Alphabet MyAlphabet where
  decode ('a':rest) = Right (rest, A)
  decode ('b':rest) = Right (rest, B)
  decode other = Left ("Invalid letter: " ++ other)

t :: [Transition MyState MyAlphabet]
t = [Q0 >==> [Q1, Q2],
     Q1 >== A ==> [Q3],
     Q2 >== A ==> [Q4],
     Q3 >== B ==> [Q5],
     Q4 >== A ==> [Q4], Q4 >== B ==> [Q6],
     Q5 >==> [Q1]]

mynfa :: NFA MyState MyAlphabet
mynfa = NFA (delta t) Q0 [Q5, Q6]