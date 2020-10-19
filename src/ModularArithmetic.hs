{-# LANGUAGE BangPatterns #-}
module ModularArithmetic where

import Control.Monad (zipWithM)

squareMultiply a p = xPow a p 1
  where
    xPow !a !p !y
      | p < 0          = 1
      | p == 0         = y
      | p == 1         = a * y
      | p `mod` 2 == 1 = xPow (a * a) (p `div` 2) (y * a)
      | p `mod` 2 == 0 = xPow (a * a) (p `div` 2) y

modPow a p n = xPow a p 1
  where
    xPow !a !p !y
      | p < 0          = 1
      | p == 0         = y `mod` n
      | p `mod` 2 == 1 = xPow ((a * a) `mod` n) (p `div` 2) ((y * a) `mod` n)
      | p `mod` 2 == 0 = xPow ((a * a) `mod` n) (p `div` 2) y

legendreSymbol a p
  | (modPow a ((p - 1) `div` 2) p) > 1 = -1
  | otherwise                          = modPow a ((p - 1) `div` 2) p
  -- a^((p - 1) `div` 2) `mod` p


squareMod a n = modPow a ((n + 1) `div` 4) n

tonelli n p = step0 n p 
  where
    step0 n p --  Step 0. Check that n is indeed a square  : (n | p) must be ≡ 1
      | (legendreSymbol n p) == 1 = step1 n p
      | otherwise = Nothing
    --  Step 1. [Factors out powers of 2 from p-1] Define q -odd- and s such as p-1 = q * 2^s 
    step1 n p = step2 n p q s
      where 
        [q, s] = (findQandS (p-1) 0)
        -- if s = 1 , i.e p ≡ 3 (mod 4) , output the two solutions r ≡ +/- n^((p+1)/4) .
        findQandS p s
            | p `rem` 2 == 0 = findQandS (div p 2) (s+1)
            | otherwise = [p,s]
    --  Step 2. Select a non-square z such as (z | p) = -1 , and set c ≡ z^q .
    step2 n p q s = step3 n p q s c
      where
        c = modPow z q p
        z = findZ p 0
        findZ !n !i
          | (legendreSymbol i n) == -1 = i
          | otherwise                  = findZ n (i+1)
    -- Step 3. Set r ≡ n ^((q+1)/2) , t ≡ n^q, m = s .
    step3 n p q s c = step4 n p m c t r
      where
        r = modPow n ((q + 1) `div` 2) p
        t = modPow n q p
        m = s
    -- Step 4. Loop. 
    -- if t ≡ 1 output r, p-r .
    --    n p m c t r
    --  Otherwise find, by repeated squaring, the lowest i , 0 < i< m , such as t^(2^i) ≡ 1
    -- Let b ≡ c^(2^(m-i-1)), and set r ≡ r*b, t ≡ t*b^2 , c ≡ b^2 and m = i.
    step4 !n !p !m !c !t !r
      | ((t - 1) `mod` p) == 0 = Just [r, p - r]
      | otherwise              = step4 n p newM newC newT newR
      where
        b = modPow c (squareMultiply 2 ((m-i)-1)) p
        newR = (r * b)      `mod` p
        newT = (t * b * b)  `mod` p
        newC = (b * b)      `mod` p
        newM = i
        i = lowestI ((t * t) `mod` p) p 1
        lowestI !t !p !i
          | ((t - 1) `mod` p) == 0  = i
          | otherwise               = lowestI ((t * t) `mod` p) p (i+1)

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd _ 0 = (1, 1, 0)
egcd a b = (a*t + b*v, t, v)
  where
    v      = s - q * t
    (_, s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: Integer -> Integer -> Integer
modInv a b =
  case egcd a b of
    (_, x, y)
      | a * x + b * y == 1 -> do
          if (x >= 0)
            then x
            else (b + x)
      | otherwise -> -1
                           

chineseRemainder residues modulii =
  ((`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues) $
    zipWith modInv crtModulii modulii
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

totientPQ p q = (p - 1) * (q - 1)

-- https://en.wikipedia.org/wiki/General_number_field_sieve

factors n = []

-- Quadratic Sieve Factoring Algorithm https://www.cs.virginia.edu/crab/QFS_Simple.pdf
-- https://vtechworks.lib.vt.edu/bitstream/handle/10919/36618/etd.pdf
-- http://www.ams.org/notices/199612/pomerance.pdf
factorsQFS = 0
  where
    f = 0

-- Pollard's Rho
factorsRho n = step1 n
  where
    step1 n
      | twos > 0  = g newN [(2, twos)]
      | otherwise = g newN []
      where
        (newN, twos) = specialDiv n 2 0
        fs           = g newN []
    g 1 fs = fs
    g n fs = g r ((d, e) : fs)
      where
        d = f n 2 2 1
        (r, e) = specialDiv n d 0
    f n a b d
      | d == 1    = f n newA newB d
      | otherwise = d
      where
        d    = gcd (newA - newB) n
        newA = ((a * a) + 1) `mod` n
        tempB = ((b * b) + 1) `mod` n
        newB = ((tempB * tempB) + 1) `mod` n
    specialDiv a b i
      | a `rem` b == 0 = specialDiv (a `div` b) b (i + 1)
      | otherwise = (a, i)
  

-- trial division
factorsTD x = f x [] 2
  where
    f n fs i
      | n == 1 = fs
      | n == i = (i:fs)
      | (n `rem` i) == 0 = f (divUntil n i) (i:fs) (i+1)
      | otherwise = f n fs (i+1)
    divUntil a b
      | a `rem` b == 0 = divUntil (a `div` b) b
      | otherwise = a


-- totientN n = totient $ factors n

totient fs = foldl1 (*) $ single <$> fs
  where
    single (n, e) = (pred n) * (squareMultiply n (pred e))

n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)

root a b = findAns $ iterate (\x -> (a1 * x + b `div` (x ^ a1)) `div` a) 1
  where
    a1 = a - 1
    findAns (x:xs@(y:z:_))
      | x == y || x == z = min y z
      | otherwise = findAns xs


cfExpansion n d = loop d [n `div` d] (mod n d)
  where
    loop _ e 0 = e
    loop d e r =
      loop r (e ++ [q]) newR
      where
        q = div d r
        newR = mod d r

convergents x = loop x [] [] 0 0 0 0
  where
    loop [] n d _ _ _ _ = (n, d)
    loop (e:es) [] []   ni di ni1 di1 =
        loop es [e] [1] e  1  0   0
    loop (e:es) [n] [d] ni di ni1 di1 =
      loop es (n:[e * ni + 1]) (d:[e]) (e*ni+1) e  ni  di
    loop (e:es) n               d         ni    di ni1 di1 =
      loop es (n ++ [e * ni + ni1]) (d ++ [e * di + di1]) (e * ni + ni1) (e * di + di1) ni di

-- Discrete Logarithm Pohlig-Hellman algorithm
dlpPohligHellman g a n = out
  where
    factors = reverse $ factorsRho (n-1)
    out = open [] [] $ cmp <$> factors
    -- out = cmp <$> factors
    open as bs [] = chineseRemainder as bs
    open as bs ((0,_) : xs) = open (as) (bs) xs
    open as bs ((1,_) : xs) = open (as) (bs) xs
    open as bs ((a,b) : xs) = open (a:as) (b:bs) xs
    cmp (q, _) = (loop q, q)
    loop q = try ta tb 0 q n
      where
        ta = testPH g q
        tb = testPH a q
        testPH x y = modPow x ((n-1) `div` y) n
        try a b e q n
            | e > q = -1
            | (modPow a e n) == b = e
            | otherwise =  try a b (e + 1) q n

