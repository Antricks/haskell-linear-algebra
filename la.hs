import Data.List.Ordered (minus)
import Debug.Trace (trace)

-- TODO replace `undefined` calls with descriptive `error` calls where function can not be defined

-- TODO? I'm not even sure anymore if this custom datatype is even a good idea or if it's just unnecessary overhead...
-- Might rework that some time...
data Matrix a = Matrix {matH :: Int, matW :: Int, matLines :: [[a]]}

instance (Show a) => Show (Matrix a) where
  show (Matrix h w lines) = "Matrix " ++ show h ++ "×" ++ show w ++ ":" ++ concatMap showLine lines
    where
      showLine :: [a] -> String
      showLine as = "\n( " ++ tail (concatMap (("\t" ++) . show) as) ++ " )"

-- Represents a number from a finite field
data ModNum = ModNum {numMod :: Integer, value :: Integer} deriving (Eq)

modNum :: Integer -> Integer -> ModNum
modNum m x = ModNum m (x `mod` m)

instance Show ModNum where
  show (ModNum m x) = show (x `mod` m) ++ "∈𝔽" ++ show m

instance Num ModNum where
  (+) :: ModNum -> ModNum -> ModNum
  (ModNum ma a) + (ModNum mb b)
    | ma == mb = ModNum ma ((a + b) `mod` ma)
    | otherwise = undefined
  (*) :: ModNum -> ModNum -> ModNum
  (ModNum ma a) * (ModNum mb b)
    | ma == mb = ModNum ma ((a * b) `mod` ma)
    | otherwise = undefined
  negate :: ModNum -> ModNum
  negate (ModNum m x) = ModNum m (m - (x `mod` m))
  abs :: ModNum -> ModNum
  abs = id -- TODO? should this maybe rather be undefined?
  signum :: ModNum -> ModNum
  signum (ModNum m x) = ModNum m 1 -- TODO? should this maybe rather be undefined?
  fromInteger :: Integer -> ModNum
  fromInteger = undefined -- This can't be implemented here (as the module is missing)

instance Fractional ModNum where
  recip :: ModNum -> ModNum
  recip (ModNum m a)
    | m `elem` primesTo m = ModNum m (modInv a m) -- NOTE: this can only be defined where m is prime, right?
    | otherwise = undefined
  fromRational :: Rational -> ModNum
  fromRational = undefined -- This can't really be implemented in a sensible way, right?

instance (Num a) => Num (Matrix a) where
  (+) :: (Num a) => Matrix a -> Matrix a -> Matrix a
  a@(Matrix h w _) + b@(Matrix h' w' _)
    | h == h' && w == w' = zipWithMat (+) a b
    | otherwise = undefined
  (-) :: (Num a) => Matrix a -> Matrix a -> Matrix a
  a@(Matrix h w _) - b@(Matrix h' w' _)
    | h == h' && w == w' = zipWithMat (-) a b
    | otherwise = undefined
  (*) :: (Num a) => Matrix a -> Matrix a -> Matrix a
  a@(Matrix h w lines) * b@(Matrix h' w' lines') = undefined -- TODO implement
  abs :: (Num a) => Matrix a -> Matrix a
  abs = mapMatrix abs
  signum :: (Num a) => Matrix a -> Matrix a
  signum = mapMatrix signum -- NOTE: does NOT satisfy abs x * signum x = x
  fromInteger :: (Num a) => Integer -> Matrix a
  fromInteger = undefined

instance (Eq a) => Eq (Matrix a) where
  mat@(Matrix h w lines) == mat'@(Matrix h' w' lines') = h == h' && w == w' && lines == lines'

primesTo :: (Integral a) => a -> [a]
primesTo 2 = [2]
primesTo m = 2 : sieve [3, 5 .. m]
  where
    sieve (p : xs)
      | p * p > m = p : xs
      | otherwise = p : sieve (xs `minus` [p * p, p * p + 2 * p ..])

-- Given a and m, return x such that ax = 1 mod m.
-- NOTE: This still returns (maybe wrong values) for cases where g != 1
modInv :: (Integral a) => a -> a -> a
modInv a m = mkPos i
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInvMaybe :: (Integral a) => a -> a -> Maybe a
modInvMaybe a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: (Integral a) => a -> a -> (a, a, a)
gcdExt a 0 = (1, 0, a)
gcdExt a b = (t, s - q * t, g)
  where
    (q, r) = a `quotRem` b
    (s, t, g) = gcdExt b r

replaceAt :: Int -> a -> [a] -> [a]
replaceAt 0 a (x : xs) = a : xs
replaceAt n a (x : xs) = x : replaceAt (n - 1) a xs

-- Applies a binary operator (dest `op` src) between two lines, replacing dest with the result
lineOp :: (a -> a -> a) -> Int -> Int -> Matrix a -> Matrix a
lineOp op src dest mat@(Matrix h w lines) = Matrix h w (replaceAt dest (zipWith op (lines !! dest) (lines !! src)) lines)

lineAdd :: (Num a) => a -> Int -> Int -> Matrix a -> Matrix a
lineAdd c = lineOp ((+) . (* c))

lineSub :: (Num a) => a -> Int -> Int -> Matrix a -> Matrix a
lineSub c = lineOp ((-) . (* c))

lineMult :: (Num a) => a -> Int -> Matrix a -> Matrix a
lineMult c i mat@(Matrix h w lines) = Matrix h w $ replaceAt i (map (* c) (lines !! i)) lines

subMatrix :: Int -> Int -> Int -> Int -> Matrix a -> Matrix a
subMatrix fstLine fstCol subH subW mat@(Matrix h w lines) = Matrix (min subH (h-fstLine)) (min subW (w-fstCol)) $ (take subH . drop fstLine) $ map (take subW . drop fstCol) lines

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix op (Matrix h w lines) = Matrix h w (map (map op) lines)

zipWithMat :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipWithMat op (Matrix h w lines) (Matrix h' w' lines') = Matrix (min h h') (min w w') $ zipWith (zipWith op) lines lines'

transpose :: Matrix a -> Matrix a
transpose (Matrix h w lines) = Matrix w h $ foldr (zipWith (:)) (replicate w []) lines

dropTake :: Int -> Int -> [a] -> [a]
dropTake d t = take t . drop d

unitList :: a -> a -> Int -> Int -> [a]
unitList _ _ 0 _ = []
unitList zero one n i = e : unitList zero one (n - 1) (i - 1)
  where
    e = if i == 0 then one else zero

unitMatrix :: a -> a -> Int -> Matrix a
unitMatrix zero one n = Matrix n n $ map (unitList zero one n) [0 .. n - 1]

colVec :: [a] -> Matrix a
colVec as = Matrix (length as) 1 $ map (: []) as

lineVec :: [a] -> Matrix a
lineVec as = Matrix 1 (length as) [as]

insertMat :: a -> Int -> Int -> Matrix a -> Matrix a -> Matrix a
insertMat placeholder insLine insCol insMat@(Matrix insH insW insLines) origMat@(Matrix origH origW origLines) = Matrix newH newW newLines
  where
    newH = max origH (insLine + insH)
    newW = max origW (insCol + insW)
    newLines =
      [ [ if line `elem` [insLine .. insLine + insH - 1] && col `elem` [insCol .. insCol + insW - 1]
            then insLines !! (line - insLine) !! (col - insCol)
            else
              if line `elem` [0 .. origH - 1] && col `elem` [0 .. origW - 1]
                then origLines !! line !! col
                else placeholder
          | col <- [0 .. newW - 1]
        ]
        | line <- [0 .. newH - 1]
      ]

insertToRight :: a -> Matrix a -> Matrix a -> Matrix a
insertToRight placeholder insMat origMat@(Matrix origH origW _) = insertMat placeholder 0 origW insMat origMat

insertToLeft :: a -> Matrix a -> Matrix a -> Matrix a
insertToLeft placeholder insMat origMat = insertToRight placeholder origMat insMat

insertUnder :: a -> Matrix a -> Matrix a -> Matrix a
insertUnder placeholder insMat origMat@(Matrix origH origW _) = insertMat placeholder origH 0 insMat origMat

insertOver :: a -> Matrix a -> Matrix a -> Matrix a
insertOver placeholder insMat origMat = insertUnder placeholder origMat insMat

matrixOf :: a -> Int -> Int -> Matrix a
matrixOf elem height width = Matrix height width $ replicate height $ replicate width elem

swapTwo :: Int -> Int -> [a] -> [a]
swapTwo f s xs =
  zipWith
    ( \x y ->
        if x == f
          then xs !! s
          else
            if x == s
              then xs !! f
              else y
    )
    [0 ..]
    xs

swapLines :: Int -> Int -> Matrix a -> Matrix a
swapLines i j mat@(Matrix height width lines) = Matrix height width $ swapTwo i j lines

rrefArea :: (Eq a, Fractional a, Num a) => a -> Int -> Int -> Int -> Int -> Matrix a -> Matrix a
rrefArea zero origFstLine origFstCol origGaussH origGaussW mat@(Matrix matH matW lines) = rrefArea' zero origFstLine origFstCol origGaussH origGaussW mat
  where
    rrefArea' zero fstLine fstCol gaussH gaussW mat@(Matrix matH matW lines)
      | subMatrix fstLine fstCol gaussH gaussW mat == matrixOf zero gaussH gaussW = mat
      | otherwise = case (maybeK, maybeI) of
          (Just k, Just i) -> extendGauss $ rrefArea' zero (fstLine + 1) (fstCol + k) (gaussH - 1) (gaussW - (fstCol - k)) $ basicGauss $ swapLines fstLine i mat
            where
              -- TODO? maybe do this more elegantly... idk...
              -- Transforms area to row echelon form where every line begins with 1
              basicGauss mat'@(Matrix matH' matW' lines') = Matrix matH' matW' $ over ++ map (/ headElem) headLine : map (\line -> zipWith (\a b -> b - a * (line !! k) / headElem) headLine line) toElim ++ underElim
                where
                  over = take fstLine lines'
                  headElem = headLine !! k -- NOTE should not be zero (if it is, there's something wrong with the code)
                  headLine = lines' !! fstLine
                  toElim = take (gaussH - 1) $ drop (fstLine + 1) lines'
                  underElim = drop (fstLine + gaussH) lines'
              -- Assumes extended row echelon form under current fstLine, transforms area to unit matrix like form
              -- extendGauss mat = mat
              extendGauss mat'@(Matrix matH' matW' lines')
                | origFstLine < fstLine =
                    Matrix matH' matW' $
                      over ++ map (\line -> zipWith (\a b -> b - a * (line !! k)) headLine line) toSubFrom ++ headLine : under
                | otherwise = mat'
                where
                  over = take origFstLine lines'
                  toSubFrom = drop origFstLine $ take fstLine lines'
                  headLine = lines' !! fstLine
                  under = drop (fstLine + 1) lines'
          (Nothing, _) -> mat
          (_, Nothing) -> mat
      where
        maybeK :: Maybe Int -- TODO If I'm not mistaking, this Maybe wrap should be redundant. Let's rework that
        maybeK
          | possibleKs /= [] = Just $ head possibleKs
          | otherwise = Nothing
          where
            possibleKs = [j | j <- [fstCol .. fstCol + gaussW - 1], any (/= zero) [l !! j | l <- take gaussH $ drop fstLine lines]]
        maybeI :: Maybe Int -- NOTE/TODO this Maybe results from the one in maybeK only.
        maybeI = do
          k' <- maybeK
          Just $ head [j | j <- [fstLine .. fstLine + gaussH - 1], lines !! j !! k' /= zero]

invertMatrix :: (Eq a, Fractional a, Num a) => a -> a -> Matrix a -> Matrix a
invertMatrix zero one mat@(Matrix h w lines)
  | h == w = subMatrix 0 n n n $ rrefArea zero 0 0 n n $ insertToRight zero (unitMatrix zero one n) mat
  | otherwise = undefined
  where
    n = w

testMatrix :: Matrix Integer
testMatrix = Matrix 4 4 [[1, 2, 3, 4], [2, 3, 0, 1], [0, 0, 2, 1], [1, 3, 3, 7]]
