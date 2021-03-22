import FATypes
import Data.Set  ( Set )
import qualified Data.Set as Set
import Data.List ( nub )
import qualified Data.List as List

-- | isState1 returns if the first state in the transition function is part of the set of states
isState1 :: Ord a => Set a -> Set (Move a) -> Bool
isState1 q ms = st`Set.isSubsetOf` q
     where 
     st_e = Set.fromList [st | Emove st _ <-Set.toList ms ]
     st_m = Set.fromList [st | Move st _ _ <-Set.toList ms ]
     st   = st_m `Set.union` st_e

-- | isState1 returns if the second state in the transition function is part of the set of states
isState2 :: Ord a => Set a -> Set (Move a) -> Bool
isState2 q ms = st`Set.isSubsetOf` q
     where 
     st_e = Set.fromList [st | Emove _ st <-Set.toList ms ] 
     st_m = Set.fromList [st | Move _ _ st <-Set.toList ms ]
     st   = st_m `Set.union` st_e

-- | The accept function recieves a FA data type and returns if it is a finite automaton or not, giving the reasons behind its categorization. 
accept :: Ord a => FA a -> String
accept a 
     | Set.null q                  = "The automaton is invalid because it does not have states"
     | not (q0 `Set.member` q)     = "The automaton is invalid because q0 does not belong to Q"
     | not (isState1 q delta)      = "The automaton is invalid because its transition fuction uses states that do not exist"
     | not (isState2 q delta)      = "The automaton is invalid because its transition fuction uses states that do not exist"
     | not (f `Set.isSubsetOf` q ) = "The automaton if invalid because F is not a subset of Q"
     | otherwise                   = "The automaton is valid"
     where
     q     = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0    = faStartState a
     f     = faFinalStates a

-- | epsilonList determines if there are transition fuctions that use epsilon or not. 
epsilonList :: Eq a => Set (Move a) -> Bool
epsilonList ms = null [st | Emove st _ <-Set.toList ms ] -- If the list containing all the Emoves is empty, the transition function 
--does not use Emoves

-- | The isENFA function returns whether a FA data type is a epsilon non-deterministic finite automaton.
isENFA :: Ord a => FA a -> Bool
isENFA a
     | accept a /= "The automaton is valid" = False
     | epsilonList delta = False -- If epsilonList in delta returns True, it means the transition function does not use Emoves, therefore 
     --the automaton is not an e-NFA
     | otherwise = True
     where
     delta = faMoves a

-- | The inputNum function returns whether the list of inputs of the state's transition functions is equal to the alphabet.
inputNum :: Eq a => a -> Set (Move a) -> Bool
inputNum q ms = List.sort [ c | Move st c _ <- Set.toList ms, st == q ] == sigma
-- We use Sort in order to avoid that the order of sigma and ms afect the result. For instance, we avoid 'ab' == 'ba'.
  where
    sigma  = List.sort (nub [ c | Move _ c _ <- Set.toList ms ]) -- The automaton's alphabet

-- | The isDFA function returns whether a FA data type is a deterministic finite automaton.
isDFA :: Ord a => FA a -> Bool
isDFA a
     | accept a /= "The automaton is valid" = False
     | not (and (Set.map (\x -> inputNum x delta) q)) = False -- If at least one state's transition function repeats an input or lacks one, 
     --the automaton is not deterministic
     | isENFA a = False
     | otherwise = True
     where
     q = faStates a 
     delta = faMoves a

-- | The isNFA function returns whether a FA data type is a non- deterministic finite automaton.
isNFA :: Ord a => FA a -> Bool
isNFA a
     | accept a /= "The automaton is valid" = False
     | isDFA a   = False
     | isENFA a  = False
     | otherwise = True

-- |  Validation returns a String that explains what type of automaton is the input.
validation :: Ord a => FA a -> String
validation a
     | isDFA a   = "The automaton is a DFA"
     | isENFA a  = "The automaton is an e-NFA"
     | isNFA a   = "The automaton is a NFA"
     | otherwise = " "

main :: IO ()
main = do
     file <- readFile "mach-n.txt"
     let fa =read file :: FA Int
     print(accept fa)
     print(validation fa)