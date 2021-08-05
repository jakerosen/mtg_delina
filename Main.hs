module Main where

import Control.Lens
import Control.Monad.Loops (iterateWhile)
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Vector.Mutable as MVec
import System.Random
import Text.Printf

main :: IO ()
main = do
  let maxRolls = 50
      maxIter = 10000
      startPixies = 1
  (x, vec) <- activateDelina maxRolls maxIter startPixies
  for_ (Vec.indexed vec) \(i, numToks) -> do
    printf "%d: %d\n" i numToks
  print x

-- Delina rolls a d20 and gets 1 token on 1-14 and gets 1 token + reroll on
-- 15-20.  So there's a 6/20 chance to get a reroll, and obviously Pixies
-- improve that chance.
--
-- I'm going to have to make a special case (if I bother) for 0 Pixies because
-- you can't copy a Pixie if you don't have one.  If you do have a Pixie, I'm
-- assuming that you copy it because that improves your odds of future rerolls.
activateDelina ::
  Int -> -- max rolls per iteration
  Int -> -- number of iterations
  Int -> -- number of Pixies
  IO
    ( Double {- avg number of tokens -},
      Vector Int {- number of iterations
                     that got this number of tokens -}
    )
activateDelina maxRolls maxIter startPixies = do
  runAll <$> newStdGen
  where
    -- state is (current iter, running total of toks, vector of tok info)
    allFinalState :: RandomGen g => State (g, Int, Int, Vector Int) Int
    allFinalState = iterateWhile (< maxIter) do
      (g, i, total, vec) <- get
      let (g1, g') = split g
          numToks = runIter g1
          i' = i + 1
          total' = total + numToks
          vec' = Vec.modify (\v -> MVec.modify v (+ 1) numToks) vec
      put (g', i', total', vec')
      return i'

    runAll :: RandomGen g => g -> (Double, Vector Int)
    runAll g =
      let (_, _, totalToks, vec) =
            execState
              allFinalState
              (g, 0, 0, Vec.replicate (maxRolls + 1) 0)
          avg = fromIntegral totalToks / fromIntegral maxIter
       in (avg, vec)

    rollOnce :: RandomGen g => State (g, Int, Int) Int
    rollOnce = do
      (_, numToks, _) <- get
      rolls :: [Int] <-
        traverse
          (\_ -> randomRState (1, 20))
          (replicate (1 + startPixies + numToks) undefined)
      return (maximum rolls)

    randomRState ::
      RandomGen g =>
      (Int, Int) ->
      State (g, Int, Int) Int -- random value generated
    randomRState (lo, hi) = do
      (g, _, _) <- get
      let (x, g') = randomR (lo, hi) g
      modify (\s -> s & _1 .~ g')
      return x

    condition :: (Int, Int) -> Bool
    condition (numRolls, lastRoll) =
      numRolls == 0 || (numRolls < maxRolls && lastRoll >= 15)

    -- State values are g, num toks, num rolls
    finalState :: RandomGen g => State (g, Int, Int) (Int, Int)
    finalState = iterateWhile condition do
      roll <- rollOnce
      -- increment num toks and num rolls by 1
      modify (\s -> s & _2 %~ (+ 1) & _3 %~ (+ 1))
      (_, _, numRolls) <- get
      return (numRolls, roll)

    runIter :: RandomGen g => g -> Int
    runIter g =
      let (_, numToks, _) = execState finalState (g, 0, 0)
       in numToks
