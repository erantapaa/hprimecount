module PrimeCount
 ( nth_prime
 , phi
 , pi
 , pi_deleglise_rivat
 , pi_deleglise_rivat2
 , pi_legendre
 , pi_lehmer
 , pi_lmo
 , pi_lmo2
 , pi_lmo3
 , pi_lmo4
 , pi_li
 , pi_li_inverse
 )
where

import Prelude hiding (pi)
import Foreign.C.Types
import Data.Int

convert1 :: (CLong -> CLong) -> (Int64 -> Int64)
convert1 f = fromIntegral . f . fromIntegral

convert2 :: (CLong -> CLong -> CLong) -> (Int64 -> Int64 -> Int64)
convert2 f a b = fromIntegral $ f (fromIntegral a) (fromIntegral b)

nth_prime = convert1 nth_prime'
phi = convert2 phi'
pi = convert1 pi'
pi_deleglise_rivat = convert1 pi_deleglise_rivat'
pi_deleglise_rivat2 = convert1 pi_deleglise_rivat2'
pi_legendre = convert1 pi_legendre'
pi_lehmer = convert1 pi_lehmer'
pi_lmo = convert1 pi_lmo'
pi_lmo2 = convert1 pi_lmo2'
pi_lmo3 = convert1 pi_lmo3'
pi_lmo4 = convert1 pi_lmo4'
pi_li = convert1 pi_li'
pi_li_inverse = convert1 pi_li_inverse'

foreign import ccall "_ZN10primecount9nth_primeEx"
  nth_prime' :: CLong -> CLong

foreign import ccall "_ZN10primecount3phiExx"
  phi' :: CLong -> CLong -> CLong

foreign import ccall "_ZN10primecount2piEx"
  pi' :: CLong -> CLong

foreign import ccall "_ZN10primecount18pi_deleglise_rivatEx"
  pi_deleglise_rivat' :: CLong -> CLong

foreign import ccall "_ZN10primecount18pi_deleglise_rivatEx"
  pi_deleglise_rivat2' :: CLong -> CLong

foreign import ccall "_ZN10primecount11pi_legendreEx"
  pi_legendre' :: CLong -> CLong

foreign import ccall "_ZN10primecount9pi_lehmerEx"
  pi_lehmer' :: CLong -> CLong

foreign import ccall "_ZN10primecount6pi_lmoEx"
  pi_lmo' :: CLong -> CLong

foreign import ccall "_ZN10primecount7pi_lmo2Ex"
  pi_lmo2' :: CLong -> CLong

foreign import ccall "_ZN10primecount7pi_lmo3Ex"
  pi_lmo3' :: CLong -> CLong

foreign import ccall "_ZN10primecount7pi_lmo4Ex"
  pi_lmo4' :: CLong -> CLong

foreign import ccall "_ZN10primecount2LiEx"
  pi_li' :: CLong -> CLong

foreign import ccall "_ZN10primecount10Li_inverseEx"
  pi_li_inverse' :: CLong -> CLong

