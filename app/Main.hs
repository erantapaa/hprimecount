{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import qualified PrimeCount as PC
import Math.NumberTheory.Primes
import System.TimeIt
import System.Environment
import Text.Printf
import Data.Int
import Data.List (foldl', tails)

sum' :: [Integer] -> Integer
sum' = foldl' (+) 0

sum'' :: [Int64] -> Int64
sum'' = foldl' (+) 0

-- e501a - solve e501 with arithmoi

e501a n = e501a1 n + fromIntegral (e501a2 n)

e501a1 n = sum' $ do
  ((i,p):ps) <- takeWhile (\((_,p):_) -> p^3 <= n) (tails $ zip [(1::Int)..] primes)
  let t = div n (p^3)
      da = primeCount t
      d1 = if t >= p then (-1) else 0
      db = sum' $ do (j,q) <- takeWhile (\(_,q) -> p*q*q < n) ps
                     let x = p*q
                         !db = primeCount (div n x) - fromIntegral j
                     return db
      !s = da + d1 + db
  return s

e501a2 n = length [ p | p <- takeWhile (\p -> p^7 <= n) primes ]

-- e501b - solve e501 with primecount

e501b n = e501b1 n + fromIntegral (e501b2 n)

e501b1 n = sum'' $ do
  ((i,p):ps) <- takeWhile (\((_,p):_) -> p^3 <= n) (tails $ zip [(1::Int)..] primes)
  let t = div n (p^3)
      da = PC.pi_deleglise_rivat2(fromIntegral t)
      d1 = if t >= p then (-1) else 0
      db = sum'' $ do (j,q) <- takeWhile (\(_,q) -> p*q*q < n) ps
                      let x = p*q
                          !db = PC.pi_deleglise_rivat2(fromIntegral $ div n x) - fromIntegral j
                      return db
      !s = da + d1 + db
  return s

e501b2 n = length [ p | p <- takeWhile (\p -> p^7 <= n) primes ]

cmpMain :: [String] -> IO ()
cmpMain args = do
  forM_ args $ \a -> do
    let n = read a
    (t0, c0) <- timeItT $ let !x = PC.pi_deleglise_rivat n in return x
    putStrLn $ printf "n = %d, t = %7.3f - prime count: %d" n t0 c0
    (t1, c1) <- timeItT $ let !x = primeCount (fromIntegral n) in return x
    putStrLn $ printf "n = %d, t = %7.3f - arithmoi   : %d" n t1 c1

readN :: String -> (Integer, String)
readN a = 
  let n = read a in
  if n < 15
    then (10^n, "1e" ++ printf "%-2d" n )
    else (n, show n)

-- run the 501 solver using arithmoi
run501a :: [String] -> IO ()
run501a args = do
  forM_ args $ \a -> do
    let (n, nstr) = readN a
    (t, c) <- timeItT $ let !x = e501a n in return x
    putStrLn $ printf "t = %8.3f - n = %s - answer: %12d" t nstr c

-- run the 501 solver using primecount
run501b :: [String] -> IO ()
run501b args = do
  forM_ args $ \a -> do
    let (n, nstr) = readN a
    (t, c) <- timeItT $ let !x = e501b n in return x
    putStrLn $ printf "t = %8.3f - n = %s - answer: %12d" t nstr c

main = do
  (cmd : args) <- getArgs
  case cmd of
    "cmp"  -> cmpMain args
    "501a" -> run501a args
    "501b" -> run501b args
    _      -> error "first arg should be: cmp, 501a or 501b"

