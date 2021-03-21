import FATypes
import Data.Set  ( Set )
import qualified Data.Set as Set
import Data.List ( nub )
import qualified Data.List as List


-- | The accept function recieves a FA data type and returns if it is a finite automaton or not, giving the reasons behind its categorization. 
accept :: Ord a => FA a -> String
accept a 
     | Set.null q = "The automaton is invalid because it does not have states"
     | q0 `Set.member` q == False = "The automaton is invalid because q0 does not belong to Q"
     | f `Set.isSubsetOf` q == False = "The automaton if invalid because F is not a subset of Q"
     | otherwise = "The automaton is valid"

     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a


-- | epsilonList determines if there are epsilon transition fuctions or not. 
epsilonList :: Eq a => Set (Move a) -> Bool
epsilonList ms = null [st | Emove st _ <-Set.toList ms ] 



-- | The isENFA function returns whether a FA data type is a epsilon non-deterministic finite automaton.
isENFA :: Ord a => FA a -> Bool
isENFA a
     | accept a /= "The automaton is valid" = False
     | epsilonList delta = False
     | otherwise = True

     where
     delta = faMoves a


-- | The inputNum function returns whether the number of Moves that a state is the same as the number of symbols in the alphabet.
inputNum :: Eq a => a -> Set (Move a) -> Bool
inputNum q ms = List.sort [ c | Move st c _ <- Set.toList ms, st == q ] == sigma
  where
    -- We use Sort in order to avoid that the order of sigma and ms afect the result. For instance, we avoid 'ab' == 'ba'.
    sigma  = List.sort (nub [ c | Move _ c _ <- Set.toList ms ]) -- | Automaton's alphabet

-- | The isDFA function returns whether a FA data type is a deterministic finite automaton.
isDFA :: Ord a => FA a -> Bool
isDFA a
     | accept a /= "The automaton is valid" = False
     | and (Set.map (\x -> inputNum x delta) q) == False = False
     | isENFA a = False
     | otherwise = True

     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a


-- | The isNFA function returns whether a FA data type is a non- deterministic finite automaton.
isNFA :: Ord a => FA a -> Bool
isNFA a
     | accept a /= "The automaton is valid" = False
     | isDFA a = False
     | isENFA a = False
     | otherwise = True
 
     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a
     eps = Emove a a

-- |  Validation returns a String that explains what type of automaton is the input.
validation :: Ord a => FA a -> String
validation a
     | isDFA a = "The automaton is a DFA"
     | isENFA a = "The automaton is an e-NFA"
     | isNFA a = "The automaton is a NFA"

main :: IO ()
main = do
     let fa = MkFA (Set.fromList [0,1,2,3])(Set.fromList [ Move 0 'a' 1, Move 0 'a' 2, Move 1 'a' 3, Move 1 'b' 3, Move 2 'a' 3, Move 2 'b' 3, Move 3 'b' 3, Move 3 'a' 2])0(Set.fromList [3])
     print(accept fa)
     print(validation fa)

 {- file <- readFile "mach-m.txt"
  let test2 = read file :: FA String -}