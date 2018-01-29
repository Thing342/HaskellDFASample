module NFA (Alphabet(..),
Transition, 
NFA(..), check, 
(==>), (>==), (>==>), delta
) where
  import Control.Monad
  import Data.List

  class Alphabet a where
    decode :: String -> Either String (String, a)
  
  type Transition q a = q -> Maybe a -> [q]
  
  data NFA q a = NFA {
    nfa_delta :: Transition q a,
    nfa_q0 :: q,
    nfa_qf :: [q]
  }
  
  advance :: (Alphabet a) => String -> Either String [(String, Maybe a)]
  advance str = do
    (rest, sym) <- decode str
    return [(rest, Just sym), (str, Nothing)]
  
  parse :: (Alphabet a) => NFA q a -> String -> Either String [q]
  parse (NFA _ state _) [] = Right [state]
  parse nfa str = do 
    cases <- advance str
    let p = [parse_next nfa rest symbol | (rest, symbol) <- cases]
    result <- sequence p
    return $ join result
  
  parse_next :: (Alphabet a) => NFA q a -> String -> Maybe a -> Either String [q]
  parse_next (NFA d state f) rest symbol = do
    let newstates = d state symbol
    let next = [parse (NFA d q1 f) rest | q1 <- newstates]
    result <- sequence next
    return $ join result
  
  check :: (Alphabet a, Eq q, Show q) => NFA q a -> String -> Either String [q]
  check nfa str = do 
    res <- parse nfa str --Traverse NFA
    case intersect res (nfa_qf nfa) of
      [] -> Left $ "No path to final state! " ++ (show res)
      good -> Right good
  
  (==>) :: (Eq a, Eq q) => (q, a) -> [q] -> Transition q a
  (q0, a0) ==> q1 = let
    tr qn (Just an) = if (qn == q0) && (an == a0) then q1 else []
    tr _ _ = []
    in tr
  
  (>==) :: q -> a -> (q, a)
  q0 >== a0 = (q0, a0) 
  
  (>==>) :: (Eq a, Eq q) => q -> [q] -> Transition q a
  q0 >==> q1 = let
    tr qn (Nothing) = if (qn == q0) then q1 else []
    tr _ _ = []
    in tr
  
  delta :: (Eq a, Eq q) => [Transition q a] -> Transition q a
  delta table state symbol = join [d state symbol | d <- table]