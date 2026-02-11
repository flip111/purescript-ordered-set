module Test.Main where

import Prelude hiding (map)

import Data.Array as Array
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, all, elem, foldl)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype)
import Data.Set.Ordered (OSet, fromFoldable, toUnfoldable, empty, isEmpty, singleton, cons, snoc, insert, insertBy, delete, deleteWith, head, last, tail, init, uncons, unsnoc, length, sort, sortBy, sortWith, reverse, take, takeEnd, takeWhile, drop, dropEnd, dropWhile, filter, filterA, concat, concatMap, map, mapMaybe, catMaybes, mapWithIndex, union, unionBy, difference, intersect, intersectBy, subset, properSubset, findMin, findMax, null, range, index, elemIndex, elemLastIndex, findIndex, findLastIndex, insertAt, deleteAt, updateAt, updateAtIndices, modifyAt, alterAt, partition, span, slice, zipWith, zipWithA, zip, unzip, foldM, foldRecM, unsafeIndex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws.Data.Eq (checkEq)
import Test.QuickCheck.Laws.Data.Foldable (checkFoldable)
import Test.QuickCheck.Laws.Data.Monoid (checkMonoid)
import Test.QuickCheck.Laws.Data.Ord (checkOrd)
import Test.QuickCheck.Laws.Data.Semigroup (checkSemigroup)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(..))

-- Newtype wrapper to define Arbitrary without orphan instances
newtype TestOSet a = TestOSet (OSet a)

derive instance Newtype (TestOSet a) _
derive newtype instance Eq a => Eq (TestOSet a)
derive newtype instance Ord a => Ord (TestOSet a)
derive newtype instance Show a => Show (TestOSet a)
derive newtype instance Eq a => Semigroup (TestOSet a)
derive newtype instance Eq a => Monoid (TestOSet a)
derive newtype instance Foldable TestOSet

genOSet :: forall a. Arbitrary a => Eq a => Gen (OSet a)
genOSet = do
  arr <- arbitrary
  pure $ fromFoldable (Array.nubEq arr)

instance (Arbitrary a, Eq a) => Arbitrary (TestOSet a) where
  arbitrary = TestOSet <$> genOSet

hasNoDuplicates :: forall a. Eq a => OSet a -> Boolean
hasNoDuplicates s =
  let arr = toUnfoldable s :: Array a
  in Array.nubEq arr == arr

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do

  ---------------------------------------------------------------------------
  -- Typeclass laws
  ---------------------------------------------------------------------------
  describe "Typeclass laws" do
    it "Eq laws" $ liftEffect $ checkEq (Proxy :: Proxy (TestOSet Int))
    it "Ord laws" $ liftEffect $ checkOrd (Proxy :: Proxy (TestOSet Int))
    it "Semigroup laws" $ liftEffect $ checkSemigroup (Proxy :: Proxy (TestOSet Int))
    it "Monoid laws" $ liftEffect $ checkMonoid (Proxy :: Proxy (TestOSet Int))
    it "Foldable laws" $ liftEffect $ checkFoldable (Proxy :: Proxy TestOSet)

  ---------------------------------------------------------------------------
  -- Uniqueness invariant
  ---------------------------------------------------------------------------
  describe "Uniqueness invariant" do
    it "cons" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (cons x s)
    it "snoc" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (snoc s x)
    it "insert" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (insert x s)
    it "append" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (s1 <> s2)
    it "union" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (union s1 s2)
    it "difference" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (difference s1 s2)
    it "intersect" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (intersect s1 s2)
    it "concat" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) (TestOSet s3 :: TestOSet Int) ->
      let osetOfOsets = fromFoldable (Array.nubEq [ s1, s2, s3 ]) :: OSet (OSet Int)
      in hasNoDuplicates (concat osetOfOsets)
    it "concatMap" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (concatMap (\x -> cons x (singleton (x + 1))) s)
    it "filter" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (filter (_ > 0) s)
    it "sort" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (sort s)
    it "reverse" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (reverse s)
    it "take" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (take n s)
    it "drop" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (drop n s)
    it "mapMaybe" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (mapMaybe (\x -> if x > 0 then Just (mod x 3) else Nothing) s)
    it "mapWithIndex" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (mapWithIndex (\_ x -> mod x 5) s)
    it "zipWith" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (zipWith (+) s1 s2)
    it "unzip fst" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (fst (unzip (zip s1 s2)))
    it "unzip snd" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (snd (unzip (zip s1 s2)))
    it "updateAtIndices" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (updateAtIndices [ Tuple 0 99, Tuple 1 100 ] s)
    it "partition yes" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (partition (_ > 0) s).yes
    it "partition no" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (partition (_ > 0) s).no

  ---------------------------------------------------------------------------
  -- Set operations
  ---------------------------------------------------------------------------
  describe "Set operations" do
    it "union contains all elements from both" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      let u = union s1 s2
          arr1 = toUnfoldable s1 :: Array Int
          arr2 = toUnfoldable s2 :: Array Int
      in all (\x -> elem x u) arr1 && all (\x -> elem x u) arr2
    it "intersect is subset of both" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      let i = intersect s1 s2
      in subset i s1 && subset i s2
    it "difference contains nothing from second" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      let d = difference s1 s2
          arrD = toUnfoldable d :: Array Int
      in all (\x -> not (elem x s2)) arrD
    it "difference is subset of first" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      subset (difference s1 s2) s1
    it "difference with empty is identity" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      difference s empty == s
    it "intersect s s == s" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      intersect s s == s
    it "union s s == s" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      union s s == s
    it "subset is reflexive" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      subset s s
    it "properSubset is irreflexive" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      not (properSubset s s)
    it "union is commutative as a set" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      sort (union s1 s2) == sort (union s2 s1)
    it "unionBy (==) agrees with union" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      unionBy (==) s1 s2 == union s1 s2
    it "intersectBy (==) agrees with intersect" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      intersectBy (==) s1 s2 == intersect s1 s2
    it "deleteWith agrees with delete" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      deleteWith (_ == x) s == delete x s

  ---------------------------------------------------------------------------
  -- Construction and access
  ---------------------------------------------------------------------------
  describe "Construction and access" do
    it "singleton has head x and length 1" $ quickCheck \(x :: Int) ->
      head (singleton x) == Just x && length (singleton x) == 1
    it "cons then elem" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      elem x (cons x s)
    it "snoc then elem" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      elem x (snoc s x)
    it "insert then elem" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      elem x (insert x s)
    it "delete then not elem" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      not (elem x (delete x s))
    it "cons length is +1 if new, +0 if exists" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      let newLen = length (cons x s)
          oldLen = length s
      in if elem x s then newLen == oldLen else newLen == oldLen + 1
    it "fromFoldable <<< toUnfoldable is identity" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      fromFoldable (toUnfoldable s :: Array Int) == s
    it "uncons agrees with head/tail" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      case uncons s of
        Nothing -> isNothing (head s) && isNothing (tail s)
        Just { head: h, tail: t } -> head s == Just h && tail s == Just t
    it "unsnoc agrees with last/init" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      case unsnoc s of
        Nothing -> isNothing (last s) && isNothing (init s)
        Just { init: i, last: l } -> last s == Just l && init s == Just i
    it "isEmpty agrees with null" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      isEmpty s == null s
    it "isEmpty agrees with length == 0" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      isEmpty s == (length s == 0)
    it "range a a is singleton" $ quickCheck \(a :: Int) ->
      range a a == singleton a
    it "range length" $ quickCheck \(a :: Int) (n :: Int) ->
      let b = a + mod n 20
      in length (range a b) == b - a + 1
    it "map preserves uniqueness" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (map (_ + 1) s)
    it "map const collapses to at most one element" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      length (map (const 0) s) <= 1
    it "elemIndex equals elemLastIndex" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      elemIndex x s == elemLastIndex x s
    it "index after elemIndex round-trips" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      case elemIndex x s of
        Nothing -> not (elem x s)
        Just i -> index s i == Just x
    it "findIndex finds matching element" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      case findIndex (_ == x) s of
        Nothing -> not (elem x s)
        Just i -> index s i == Just x
    it "findIndex (_ == x) equals findLastIndex (_ == x)" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      findIndex (_ == x) s == findLastIndex (_ == x) s
    it "insertBy compare agrees with insert" $ quickCheck \(x :: Int) (TestOSet s :: TestOSet Int) ->
      insertBy compare x s == insert x s

  ---------------------------------------------------------------------------
  -- findMin and findMax
  ---------------------------------------------------------------------------
  describe "findMin and findMax" do
    it "findMin agrees with head of sort" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      findMin s == head (sort s)
    it "findMax agrees with last of sort" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      findMax s == last (sort s)
    it "findMin of empty is Nothing" $
      findMin (empty :: OSet Int) `shouldEqual` Nothing
    it "findMax of empty is Nothing" $
      findMax (empty :: OSet Int) `shouldEqual` Nothing
    it "findMin of singleton is Just x" $ quickCheck \(x :: Int) ->
      findMin (singleton x) == Just x
    it "findMax of singleton is Just x" $ quickCheck \(x :: Int) ->
      findMax (singleton x) == Just x

  ---------------------------------------------------------------------------
  -- JSON encode/decode
  ---------------------------------------------------------------------------
  describe "JSON encode/decode" do
    it "round-trip" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      decodeJson (encodeJson s) == Right s

  ---------------------------------------------------------------------------
  -- Show instance
  ---------------------------------------------------------------------------
  describe "Show instance" do
    it "show empty" $
      show (empty :: OSet Int) `shouldEqual` "(OSet [])"
    it "show singleton" $
      show (singleton 42) `shouldEqual` "(OSet [42])"

  ---------------------------------------------------------------------------
  -- Index-based mutations
  ---------------------------------------------------------------------------
  describe "Index-based mutations" do
    it "insertAt preserves uniqueness" $ quickCheck \(n :: Int) (x :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
          idx = if len == 0 then 0 else mod n (len + 1)
      in case insertAt idx x s of
           Nothing -> true
           Just s' -> hasNoDuplicates s'
    it "insertAt existing element returns original" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      case head s of
        Nothing -> true
        Just h -> insertAt 0 h s == Just s
    it "deleteAt reduces length by 1" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
      in if len == 0 then true
         else let idx = mod n len
              in case deleteAt idx s of
                   Nothing -> false
                   Just s' -> length s' == len - 1
    it "updateAt preserves uniqueness" $ quickCheck \(n :: Int) (x :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
      in if len == 0 then true
         else let idx = mod n len
              in case updateAt idx x s of
                   Nothing -> true
                   Just s' -> hasNoDuplicates s'
    it "modifyAt preserves uniqueness" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
      in if len == 0 then true
         else let idx = mod n len
              in case modifyAt idx (const 999) s of
                   Nothing -> true
                   Just s' -> hasNoDuplicates s'
    it "alterAt preserves uniqueness" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
      in if len == 0 then true
         else let idx = mod n len
              in case alterAt idx (\_ -> Just 999) s of
                   Nothing -> true
                   Just s' -> hasNoDuplicates s'
    it "alterAt with Nothing removes element" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
      in if len == 0 then true
         else let idx = mod n len
              in case alterAt idx (\_ -> Nothing) s of
                   Nothing -> false
                   Just s' -> length s' == len - 1
    it "unsafeIndex agrees with index" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
      in if len == 0 then true
         else let idx = mod n len
              in Just (unsafePartial (unsafeIndex s idx)) == index s idx

  ---------------------------------------------------------------------------
  -- Partition, span, and filtering
  ---------------------------------------------------------------------------
  describe "Partition, span, and filtering" do
    it "partition preserves all elements" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      let { yes, no } = partition (_ > 0) s
      in sort (yes <> no) == sort s
    it "span init <> rest equals original" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      let { init: i, rest: r } = span (_ > 0) s
      in i <> r == s
    it "filterA with pure agrees with filter" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      filterA (\x -> Just (x > 0)) s == Just (filter (_ > 0) s)
    it "mapMaybe Just is identity" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      mapMaybe Just s == s
    it "mapMaybe preserves uniqueness with collapsing function" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      hasNoDuplicates (mapMaybe (\x -> if x > 0 then Just (mod x 3) else Nothing) s)
    it "catMaybes after wrapping with Just is identity" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      let withJust = fromFoldable (Just <$> (toUnfoldable s :: Array Int)) :: OSet (Maybe Int)
      in catMaybes withJust == s
    it "mapWithIndex const collapses to at most one element" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      length (mapWithIndex (\_ _ -> 0) s) <= 1

  ---------------------------------------------------------------------------
  -- Slice and take/drop variants
  ---------------------------------------------------------------------------
  describe "Slice and take/drop variants" do
    it "slice 0 (length s) is identity" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      slice 0 (length s) s == s
    it "take n <> drop n equals original" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      take n s <> drop n s == s
    it "takeEnd n agrees with drop (length - n)" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
          n' = mod n (len + 1)
      in takeEnd n' s == drop (len - n') s
    it "dropEnd n agrees with take (length - n)" $ quickCheck \(n :: Int) (TestOSet s :: TestOSet Int) ->
      let len = length s
          n' = mod n (len + 1)
      in dropEnd n' s == take (len - n') s
    it "takeWhile <> dropWhile equals original" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      takeWhile (_ > 0) s <> dropWhile (_ > 0) s == s

  ---------------------------------------------------------------------------
  -- Sort variants
  ---------------------------------------------------------------------------
  describe "Sort variants" do
    it "sortBy compare agrees with sort" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      sortBy compare s == sort s
    it "sortWith identity agrees with sort" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      sortWith identity s == sort s

  ---------------------------------------------------------------------------
  -- Zip and unzip
  ---------------------------------------------------------------------------
  describe "Zip and unzip" do
    it "zip length is min of both" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      length (zip s1 s2) == min (length s1) (length s2)
    it "unzip reverses zip" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      let zipped = zip s1 s2
          Tuple u1 u2 = unzip zipped
          len = min (length s1) (length s2)
      in u1 == take len s1 && u2 == take len s2
    it "zipWith preserves uniqueness" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      hasNoDuplicates (zipWith (+) s1 s2)
    it "zipWithA with pure agrees with zipWith" $ quickCheck \(TestOSet s1 :: TestOSet Int) (TestOSet s2 :: TestOSet Int) ->
      zipWithA (\a b -> Just (a + b)) s1 s2 == Just (zipWith (+) s1 s2)

  ---------------------------------------------------------------------------
  -- Monadic folds
  ---------------------------------------------------------------------------
  describe "Monadic folds" do
    it "foldM with Just agrees with foldl" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      foldM (\acc x -> Just (acc + x)) 0 s == Just (foldl (+) 0 s)
    it "foldRecM with Just agrees with foldl" $ quickCheck \(TestOSet s :: TestOSet Int) ->
      foldRecM (\acc x -> Just (acc + x)) 0 s == Just (foldl (+) 0 s)
