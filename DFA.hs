module DFA (Alphabet(..), Transition, DFA(..), check, (>==), (==>), delta) where
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