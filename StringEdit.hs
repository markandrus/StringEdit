module StringEdit
  ( -- * About
    -- $about
    cost
    -- ** Example Program
    -- $results
  , main
    -- * Naive Implementation
  , naiveStringEditDist
    -- * Memoizing Implementation
  , memoStringEditDist
  , calcStringEditDist
  , stringEditDist
    -- ** @String@ Edit Recovery
  , writeStringEdit
  , stringEdit
  , showStringEdit) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.List hiding (insert)
import Data.Map as Map hiding (map, null)
import Data.Maybe
import System.Environment

{- About -}

-- $about This package provides functions for calculating a @String@ edit of
-- minimum distance between two @String@s (the notion of distance or 'cost' here
-- is synonymous with /Levenshtein distance/). That is, given two 'String's, for
-- each character, we can either associate the two according to some 'cost'
-- function or insert/delete a character from one of the 'String's with some
-- penalty (in this case, 1). Then we want to find a @String@ edit of minimum
-- distance.
--
-- Included is a naive implementation that takes exponential time, as well as a
-- memoizing implementation that takes /O(nm)/ time (where /n/ and /m/ are the
-- lengths of each word). Additionally, the memoizing version allows us to
-- recover a @String@ edit of minimum distance.

{- Results -}

-- $results An example program run:
--
-- > $ runhaskell StringEdit.hs thenameofthegame theresmyname
-- > Minimum String Edit Cost: 7.0
-- > Minimum String Edit: 
-- > thenameofthegame
-- > ther  e smy name
--
-- Also note that the minimum @String@ edit between 'A' and 'B' is the same as
-- that between 'B' and 'A', as expected.
--
-- > $ runhaskell StringEdit.hs ninakushukuru unamshukuru ninakushukuru
-- > Minimum String Edit Cost: 3.1
-- > Minimum String Edit: 
-- > ninakushukuru
-- >  unam shukuru
-- > 
-- > Minimum String Edit Cost: 3.1
-- > Minimum String Edit: 
-- >  unam shukuru
-- > ninakushukuru


-- | Calculate the cost of aligning two characters:
--
--     * Vowel-to-vowel costs 0.5,
--
--     * Vowel-to-consonant costs 1.2,
--
--     * Consonant-to-consonant costs 0.6, and
--
--     * Aligning the same characters costs 0.
cost :: Char -> Char -> Float
cost a b | a       == b       = 0
         | vowel a && vowel b = 0.5
         | vowel a || vowel b = 1.2
         | otherwise          = 0.6
  where vowel a = a `elem` "aeiou"


{- Naive Implementation -}

-- | Exponential-time implementation of the @String@ edit distance algorithm.
naiveStringEditDist :: String -> String -> Float

-- Base case: two empty @String@s.
naiveStringEditDist []     []     = 0

-- Asymmetric case: one nonempty @String@ and one empty @String@.
naiveStringEditDist (_:as) []     = 1 + naiveStringEditDist as []

-- Asymmetric case: one nonempty @String@ and one empty @String@.
naiveStringEditDist []     (_:bs) = 1 + naiveStringEditDist [] bs

-- General case: two nonempty @String@s.
naiveStringEditDist aas@(a:as) bbs@(b:bs) = minimum
  [ cost a b + naiveStringEditDist as  bs     -- Association.
  , 1        + naiveStringEditDist aas bs     -- Insertion/deletion.
  , 1        + naiveStringEditDist as  bbs ]  -- Insertion/deletion.


{- Memoizing Implementation -}

-- | Memoizing implementation of 'naiveStringEditDist'.
memoStringEditDist :: String -> String -> State (Map (String, String) Float) Float

-- Base case: two empty @String@s.
memoStringEditDist [] [] = do
  v <- gets $ Map.lookup ([], [])
  case v of
    Just d  -> return d
    Nothing -> do
      -- Associating two empty @String@s has cost 0.
      let d = 0
      modify $ insert ([], []) d
      return d

-- Asymmetric case: one nonempty @String@ and one empty @String@.
memoStringEditDist aas@(_:as) [] = do
  v <- gets $ Map.lookup (aas, [])
  case v of
    Just d  -> return d
    Nothing -> do
      -- We can only perform an insertion/deletion.
      insertionDeletion <- memoStringEditDist as []
      let d = 1 + insertionDeletion
      modify $ insert (aas, []) d
      return d

-- Asymmetric case: one nonempty @String@ and one empty @String@.
memoStringEditDist [] aas@(a:as) = do
  v <- gets $ Map.lookup (aas, [])
  case v of
    Just d  -> return d
    Nothing -> do
      -- We can only perform an insertion/deletion.
      insertionDeletion <- memoStringEditDist as []
      let d = 1 + insertionDeletion
      modify $ insert (aas, []) d
      return d

-- General case: two nonempty @String@s.
memoStringEditDist aas@(a:as) bbs@(b:bs) = do
  v <- gets $ Map.lookup (aas, bbs)
  case v of
    Just d  -> return d
    Nothing -> do
      -- We can either associate two characters, or perform one of two
      -- insertions/deletions.
      association        <- memoStringEditDist as  bs
      insertionDeletionA <- memoStringEditDist aas bs
      insertionDeletionB <- memoStringEditDist as  bbs
      let d = minimum $ [ cost a b + association
                        , 1        + insertionDeletionA
                        , 1        + insertionDeletionB ]
      modify $ insert (aas, bbs) d
      return d


-- | /O(nm)/. Calculate the @String@ edit distance of two @String@s using
-- 'memoStringEditDist' by building up the values of the memoization table from
-- back to front.
calcStringEditDist :: String -> String -> State (Map (String, String) Float) Float
calcStringEditDist a b = do
  let indices = [(x, y) | x <- reverse $ tails a
			, y <- reverse $ tails b]
  forM_ indices $ \(x, y) -> do
    d <- memoStringEditDist x y
    modify $ insert (x, y) d
  gets $ fromJust . Map.lookup (a, b)


-- | /O(nm)/. Calculate the @String@ edit distance of two @String@s using
-- 'calcStringEditDist'.
stringEditDist :: String -> String -> Float
stringEditDist a b = evalState (calcStringEditDist a b) empty


-- | /O(n)/. Trace back through a precomputed memoization table to recover the
-- edit of minimum distance between two @String@s.
writeStringEdit :: String -> String -> Map (String, String) Float -> [(Maybe Char, Maybe Char)]

-- Base case: two empty @String@s.
writeStringEdit []         []         _ = []

-- Asymmetric case: one nonempty @String@ and one empty @String@.
writeStringEdit as         []         _ = zip (map Just as) $ repeat Nothing

-- Asymmetric case: one nonempty @String@ and one empty @String@.
writeStringEdit []         bs         _ = zip (repeat Nothing) $ map Just bs

-- General case: two nonempty @String@s.
writeStringEdit aas@(a:as) bbs@(b:bs) m =
  let x = fmap (+ (cost a b)) $ Map.lookup (as,  bs) m  -- Association.
      y = fmap (+ 1)          $ Map.lookup (aas, bs) m  -- Insertion/deletion.
      z = fmap (+ 1)          $ Map.lookup (as, bbs) m  -- Insertion/deletion.
  in  if (x <= y && x <= z) then (Just a,  Just b) : writeStringEdit as  bs m else
      if (y <= x && y <= z) then (Nothing, Just b) : writeStringEdit aas bs m else
      {- (z <= x && z <= y) -}   (Just a, Nothing) : writeStringEdit as bbs m


-- | /O(nm)/. Consruct the edit of minimum distance between two @String@s using
-- 'calcStringEditDist' and 'writeStringEdit'.
stringEdit :: String -> String -> [(Maybe Char, Maybe Char)]
stringEdit a b =
  let m = execState (calcStringEditDist a b) empty
  in  writeStringEdit a b m


-- | Show a @String@ edit.
showStringEdit :: [(Maybe Char, Maybe Char)] -> String
showStringEdit xs =
  let (as, bs) = unzip xs
  in  unlines [ map (fromMaybe ' ') as
              , map (fromMaybe ' ') bs ]


{- Example Program -}

-- | Calculates the minimum @String@ edit between all adjacent pairs of
-- @String@s given.
main :: IO ()
main = do
  as <- getArgs
  unless (null as) $ do
    let wws@(_:ws) = as
        ps         = zip wws ws
    forM_ ps $ \(a, b) -> do
      let (d, m) = runState (calcStringEditDist a b) empty
      putStrLn $ "Minimum String Edit Cost: " ++ show d
      putStrLn $ "Minimum String Edit: \n" ++ (showStringEdit $ writeStringEdit a b m)
