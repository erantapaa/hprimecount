{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad
import qualified PrimeCount as PC
import Math.NumberTheory.Primes
import System.TimeIt
import System.Environment
import Text.Printf

test1 = all cmp $ map (*1000) [1..10000]
  where cmp a = fromIntegral (PC.pi_deleglise_rivat a) == primeCount (fromIntegral a)

main2 :: IO ()
main2 = do
  forM_  [1..1000] $ \i -> do
    let n = i*10000
    putStrLn $ "n = " ++ show n
    putStrLn $ "  arithmoi: "   ++ show (primeCount n)
    putStrLn $ "  primecount: " ++ show (PC.pi_deleglise_rivat (fromIntegral n))

main = do
  args <- getArgs
  forM_ args $ \a -> do
    let n = read a
    (t0, c0) <- timeItT $ let !x = PC.pi_deleglise_rivat n in return x
    putStrLn $ printf "n = %d, t = %7.3f - prime count: %d" n t0 c0

    (t1, c1) <- timeItT $ let !x = primeCount (fromIntegral n) in return x
    putStrLn $ printf "n = %d, t = %7.3f - arithmoi   : %d" n t1 c1

