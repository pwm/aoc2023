module AoC.Lib.Interval where

import AoC.Lib.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

data IV a = IV {lo :: a, hi :: a}
  deriving stock (Show, Generic)

instance (Eq a) => Eq (IV a) where
  (==) :: IV a -> IV a -> Bool
  (==) = equals

mkIV :: (Ord a) => (a, a) -> Maybe (IV a)
mkIV (a, b)
  | a < b = Just (IV a b)
  | otherwise = Nothing

idiff :: (Ord a) => IV a -> IV a -> [IV a]
idiff a b = catMaybes [mkIV (a.lo, a.hi `min` b.lo), mkIV (a.lo `max` b.hi, a.hi)]

iunions :: forall a. (Ord a) => [IV a] -> [IV a]
iunions = fixpoint go . sortOn (.lo)
  where
    go :: [IV a] -> [IV a]
    go [] = []
    go [a] = [a]
    go (a : b : ivs) = case a `imerge` b of
      Just ab -> ab : go ivs
      _ -> a : go (b : ivs)

icuts :: (Ord a) => [IV a] -> Maybe (IV a)
icuts [] = Nothing
icuts [_] = Nothing
icuts (a : as) = foldM icut a as

icut :: (Ord a) => IV a -> IV a -> Maybe (IV a)
icut a b
  | a `overlapping` b = Just (IV (a.lo `max` b.lo) (a.hi `min` b.hi))
  | otherwise = Nothing

imerges :: (Ord a) => [IV a] -> Maybe (IV a)
imerges [] = Nothing
imerges (a : as) = foldM imerge a as

imerge :: (Ord a) => IV a -> IV a -> Maybe (IV a)
imerge a b
  | a `touching` b = Just (IV (a.lo `min` b.lo) (a.hi `max` b.hi))
  | otherwise = Nothing

-------------------------------------------------------------------------------
-- Utils

touching :: (Ord a) => IV a -> IV a -> Bool
touching a b = not (a `precedes` b || a `precededBy` b)

overlapping :: (Ord a) => IV a -> IV a -> Bool
overlapping a b = a `touching` b && not (a `meets` b || a `metBy` b)

-- >>> (ibounds [], ibounds [IV 1 2], ibounds [IV 1 2, IV 3 4])
-- (Nothing,Just (1,2),Just (1,4))
ibounds :: (Ord a) => [IV a] -> Maybe (a, a)
ibounds [] = Nothing
ibounds ivs =
  let loBound = (.lo) . head . sortOn (.lo)
      hiBound = (.hi) . head . sortOn (Down . (.hi))
   in Just (loBound ivs, hiBound ivs)

-- >>> (IV 1 2 <+> 1, IV 1 2 <-> 1)
-- (IV {lo = 2, hi = 3},IV {lo = 0, hi = 1})
(<+>), (<->) :: (Num a) => IV a -> a -> IV a
i <+> n = IV (i.lo + n) (i.hi + n)
i <-> n = IV (i.lo - n) (i.hi - n)

-------------------------------------------------------------------------------
-- The 13 base relations, see:
-- https://en.wikipedia.org/wiki/Allen%27s_interval_algebra

--  |--a--|
--           |--b--|
precedes :: (Ord a) => IV a -> IV a -> Bool
precedes a b = a.hi < b.lo

--           |--a--|
--  |--b--|
precededBy :: (Ord a) => IV a -> IV a -> Bool
precededBy a b = b.hi < a.lo

--  |--a--|
--        |--b--|
meets :: (Eq a) => IV a -> IV a -> Bool
meets a b = a.hi == b.lo

--        |--a--|
--  |--b--|
metBy :: (Eq a) => IV a -> IV a -> Bool
metBy a b = b.hi == a.lo

--  |--a--|
--     |--b--|
overlaps :: (Ord a) => IV a -> IV a -> Bool
overlaps a b = a.lo < b.lo && a.hi < b.hi && b.lo < a.hi

--     |--a--|
--  |--b--|
overlappedBy :: (Ord a) => IV a -> IV a -> Bool
overlappedBy a b = b.lo < a.lo && b.hi < a.hi && a.lo < b.hi

--  |--a--|
--  |----b----|
starts :: (Ord a) => IV a -> IV a -> Bool
starts a b = a.lo == b.lo && a.hi < b.hi

--  |----a----|
--  |--b--|
startedBy :: (Ord a) => IV a -> IV a -> Bool
startedBy a b = a.lo == b.lo && b.hi < a.hi

--      |--a--|
--  |----b----|
finishes :: (Ord a) => IV a -> IV a -> Bool
finishes a b = b.lo < a.lo && a.hi == b.hi

--  |----a----|
--      |--b--|
finishedBy :: (Ord a) => IV a -> IV a -> Bool
finishedBy a b = a.lo < b.lo && a.hi == b.hi

--    |--a--|
--  |----b----|
during :: (Ord a) => IV a -> IV a -> Bool
during a b = b.lo < a.lo && a.hi < b.hi

--  |----a----|
--    |--b--|
contains :: (Ord a) => IV a -> IV a -> Bool
contains a b = a.lo < b.lo && b.hi < a.hi

--  |--a--|
--  |--b--|
equals :: (Eq a) => IV a -> IV a -> Bool
equals a b = a.lo == b.lo && a.hi == b.hi

-------------------------------------------------------------------------------
-- Pretty Print

{-
λ let ivs = [IV 9 15, IV 34 40, IV 1 6, IV 22 32, IV 12 19, IV 33 41, IV 18 28, IV 2 8]
λ putStr $ ppv ivs
          |-----|
                                  |-----|
  |----|
                      |---------|
            |------|
                                  |-------|
                  |---------|
  |-----|

λ putStr $ ppv $ iunions ivs
  |------|
          |----------------------|
                                  |-------|
-}
ppv :: [IV Int] -> String
ppv ivs =
  foldl' (\acc s -> acc <> s <> "\n") "\n"
    . map snd
    . sort
    . map (second ppIV)
    . sortOn ((.lo) . snd)
    . zip [0 :: Int ..]
    . map (<-> minimum (map (.lo) ivs))
    $ ivs
  where
    ppIV :: IV Int -> String
    ppIV i
      | i.lo == i.hi = rpad i.lo "" <> "|"
      | otherwise = rpad i.lo "" <> "|" <> replicate (i.hi - i.lo - 1) '-' <> "|"

-------------------------------------------------------------------------------
-- QuickCheck

instance (Arbitrary a, Ord a) => Arbitrary (IV a) where
  arbitrary :: Gen (IV a)
  arbitrary = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= a)
    pure $ IV (min a b) (max a b)

-- hspec spec
spec :: Spec
spec = do
  describe "imerge" $ do
    prop "idempotent" $ do
      withMaxSuccess 1000 $ \(a :: IV Int) ->
        a `imerge` a == Just a

  describe "imerges" $ do
    prop "idempotent" $ do
      withMaxSuccess 1000 $ \(a :: IV Int) ->
        imerges (replicate 10 a) == Just a
    prop "contracting" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        length (imerges ivs) <= length ivs
    prop "has stable bounds when all touching" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        not (null (imerges ivs)) ==> ibounds ivs == ibounds (maybeToList $ imerges ivs)

  describe "icut" $ do
    prop "idempotent" $
      withMaxSuccess 1000 $ \(a :: IV Int) ->
        a `icut` a == Just a
    prop "commutative" $
      withMaxSuccess 1000 $ \(a :: IV Int, b) ->
        a `icut` b == b `icut` a
    prop "associative" $
      withMaxSuccess 1000 $ \(a :: IV Int, b, c) ->
        (icut a b >>= icut c) == (icut a =<< icut b c)
    prop "has direction-independent folding" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        let as = take 1 ivs; bs = drop 1 ivs
         in foldr (mapMaybe . icut) as bs == foldl' (\acc -> flip mapMaybe acc . icut) as bs

  describe "icuts" $ do
    prop "idempotent" $
      withMaxSuccess 1000 $ \(a :: IV Int) ->
        icuts (replicate 10 a) == Just a
    prop "contracting" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        length (icuts ivs) <= length ivs
    prop "generally commutative" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        forAll (shuffle ivs) $ \sivs ->
          icuts ivs == icuts sivs
    prop "associative" $
      withMaxSuccess 1000 $ \(a :: IV Int, b, c) ->
        icuts (maybeToList (icuts [a, b]) <> [c]) == icuts ([a] <> maybeToList (icuts [b, c]))
    prop "has contracting bounds" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        case (ibounds ivs, ibounds (maybeToList $ icuts ivs)) of
          (Just (lo, hi), Just (clo, chi)) -> lo <= clo && chi <= hi
          _ -> True

  describe "iunions" $ do
    prop "idempotent" $
      withMaxSuccess 1000 $ \(a :: IV Int) ->
        iunions (replicate 10 a) == [a]
    prop "contracting" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        length (iunions ivs) <= length ivs
    prop "generally commutative" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        forAll (shuffle ivs) $ \sivs ->
          iunions ivs == iunions sivs
    prop "associative" $
      withMaxSuccess 1000 $ \(a :: IV Int, b, c) ->
        iunions (iunions [a, b] <> [c]) == iunions ([a] <> iunions [b, c])
    prop "has stable bounds" $
      withMaxSuccess 1000 $ \(ivs :: [IV Int]) ->
        ibounds ivs == ibounds (iunions ivs)

  describe "idiff" $ do
    prop "non-overlapping cases are idempotent" $
      withMaxSuccess 1000 $ \(a :: IV Int, b) ->
        not (overlapping a b) ==> idiff a b == [a] && idiff b a == [b]
    prop "contains results in 2 disjoint intervals" $
      withMaxSuccess 1000 $ \(a :: IV Int, b) ->
        contains a b ==> length (idiff a b) == 2 && null (imerges (idiff a b))
    prop "starts|finishes|during|equals are empty cases" $
      withMaxSuccess 1000 $ \(a :: IV Int, b) ->
        starts a b || finishes a b || during a b || equals a b ==>
          null (idiff a b)
    prop "overlaps|overlappedBy|startedBy|finishedBy are singleton cases" $
      withMaxSuccess 1000 $ \(a :: IV Int, b) ->
        overlaps a b || overlappedBy a b || startedBy a b || finishedBy a b ==>
          length (idiff a b) == 1
