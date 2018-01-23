import Data.Maybe

class Alphabet a where
    decode :: String -> Either String (String, a)    

type Transition q a = q -> a -> Maybe q

data DFA q a = DFA (Transition q a) q [q]

parse :: (Alphabet a) => DFA q a -> String -> Either (q, String) q
parse (DFA _ state _) [] = Right state
parse (DFA delta state f) str = case decode str of
    Left err -> Left (state, err)
    Right (rest, symbol) -> case delta state symbol of
        Nothing -> Left (state, "No transition for: " ++ str)
        Just newstate -> parse (DFA delta newstate f) rest 

check :: (Alphabet a, Eq q) => DFA q a -> String -> Either (q, String) q
check dfa@(DFA _ _ qf) str = case parse dfa str of
    Right finstate -> if finstate `elem` qf then Right finstate else Left (finstate, "Bad final state")
    Left err -> Left err

(==>) :: (Eq a, Eq q) => (q, a) -> q -> Transition q a
(q0, a0) ==> q1 = \qn an -> if (qn == q0) && (an == a0) then Just q1 else Nothing

(>==) :: q -> a -> (q, a)
q0 >== a0 = (q0, a0) 

delta :: (Eq a, Eq q) => [Transition q a] -> Transition q a
delta trans state symb = let
    matches = [y | x <- trans, let y = x state symb, isJust y ]
    in case matches of
        [] -> Nothing
        (m:_) -> m

------

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
