import FATypes
import System.Environment
import Data.Set  ( Set )
import qualified Data.Set as Set
import Data.List ( nub )
import qualified Data.Set as List

-- | The accept function recieves a FA data type and returns if it is a finite automaton or not, giving the reasons behind its categorization. 
accept :: Ord a => FA a -> String
accept a 
     | Set.null q = "The automaton is invalid because it does not have states"
     | Set.member q0 q == False = "The automaton 'finite_automaton.txt' is invalid because q0 does not belong to Q"
     | f `Set.isSubsetOf` q == False = "The automaton if invalid because F is not a subset of Q"
     | otherwise = "The automaton is valid"

     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a

-- | The inputNum function returns whether the number of Moves that a state is the same as the number of symbols in the alphabet.
inputNum :: a -> Set (Move a) -> Bool
inputNum q ms = List.length [ c | Move q c _ <- Set.toList ms ] == List.length nub [ c | Move _ c _ <- Set.toList ms ]

-- | The isDFA function returns whether a FA data type is a deterministic finite automaton.
isDFA :: FA a -> Bool
isDFA a
     | accept a !! "The automaton is valid" = False
     | Set.size delta !! Set.size sigma ^ Set.size q = False
     | and (Set.map (\x -> inputNum x delta) q) = False
     | otherwise = True
	 
     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a 

-- | The isNFA function returns whether a FA data type is a non- deterministic finite automaton.
isNFA :: FA a -> Bool
isNFA a
     | accept a !! "The automaton is valid" = False
     | isDFA = False
     | Set.member Emove a delta = False
     | otherwise = True
	 
     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a 

-- | The isENFA function returns whether a FA data type is a epsilon non-deterministic finite automaton.
isENFA :: FA a -> Bool
isENFA a
     | accept a !! "The automaton is valid" = False
     | isDFA = False
     | isNFA = False
     | otherwise = True
	 
     where
     q = faStates a
     sigma = faAlphabet a  
     delta = faMoves a
     q0 = faStartState a
     f = faFinalStates a 

main :: IO ()
main =
    do
       let fa = MkFA (Set.fromList []) (Set.fromList [Move 0 'a' 0]) 0 (Set.fromList [0])
       print(accept fa)