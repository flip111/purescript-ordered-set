module Data.Set.Ordered
  ( OSet
  , fromFoldable
  , toUnfoldable
  , singleton
  , (..), range
  -- , some
  -- , many

  , null
  , length

  , (:), cons
  , snoc
  , insert
  , insertBy

  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc

  , (!!), index
  , elemIndex
  , elemLastIndex
  , findIndex
  , findLastIndex
  , insertAt
  , deleteAt
  , updateAt
  , updateAtIndices
  , modifyAt
  , alterAt

  , reverse
  , concat
  -- , concatMap
  , filter
  , partition
  , filterA
  , mapMaybe
  , catMaybes
  , mapWithIndex

  , sort
  , sortBy
  , sortWith
  , slice
  , take
  , takeWhile
  , drop
  , dropWhile
  , span
  -- , group
  -- , group'
  -- , groupBy

  , union
  , unionBy
  , delete
  , deleteBy

  , (\\), difference
  , intersect
  , intersectBy

  , zipWith
  , zipWithA
  , zip
  , unzip

  , foldM
  , foldRecM

  , unsafeIndex

  , module Exports
  ) where

-- Data.Array has these functions, which don't make sense to have here:
-- nub, nubBy, replicate

-- Todo:
-- some, many, concatMap, group, group', groupBy
-- Functor, Apply, Bind, Traversable

import Control.Alternative (class Alternative)
import Control.Applicative (class Applicative)
import Control.Apply (class Apply, apply)
import Control.Bind (class Bind)
import Control.Lazy (class Lazy)
import Control.Monad (class Monad)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as A
import Data.Eq (class Eq)
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.Foldable (foldl, foldr, foldMap, fold, intercalate, elem, notElem, find, findMap, any, all) as Exports
import Data.Functor (class Functor, map)
import Data.Maybe (Maybe)
import Data.NaturalTransformation (type (~>))
import Data.NonEmpty (NonEmpty)
import Data.Ord (class Ord)
import Data.Ordering (Ordering)
import Data.Semigroup (class Semigroup, append)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Traversable (scanl, scanr) as Exports
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (class Unfoldable)
import Prelude (($), (<<<), (<$>))
import Data.Array.NonEmpty (NonEmptyArray)

newtype OSet a = OSet (Array a)

-- https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Functor
-- instance functorOSet :: Functor (OSet a) where
--   map f (OSet xs) = OSet $ A.nubEq $ map f xs

-- https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Control.Apply
-- instance applyOSet :: Eq a => Apply (OSet a) where
--   apply (OSet f) (OSet xs) = OSet $ A.nubEq $ apply f xs
  
-- https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Control.Bind
-- instance bindOSet :: Bind OSet where
--   bind (OSet xs) f = -- not sure how to implement this

-- https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semigroup
instance semigroupOSet :: Eq a => Semigroup (OSet a) where
  append (OSet xs) (OSet ys) = OSet $ A.nubEq $ append xs ys

-- https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Foldable
instance foldableOSet :: Foldable OSet where
  foldr f x (OSet ys) = foldr f x ys
  foldl f x (OSet ys) = foldl f x ys
  foldMap f (OSet xs) = foldMap f xs

-- https://pursuit.purescript.org/packages/purescript-foldable-traversable/4.1.1/docs/Data.Traversable
-- instance traversableOSet :: Traversable (OSet a) where
--   traverse f (OSet xs) = OSet <$> traverse f xs
--   sequence (OSet xs) = OSet <$> sequence xs

-- derive newtype instance lazyOSet :: Lazy (OSet Array)

fromFoldable :: forall f. Foldable f => f ~> OSet
fromFoldable = OSet <<< A.fromFoldable

toUnfoldable :: forall f. Unfoldable f => OSet ~> f
toUnfoldable (OSet xs) = A.toUnfoldable xs

singleton :: forall a. a -> OSet a
singleton a = OSet [a]

range :: Int -> Int -> OSet Int
range a b = OSet $ A.range a b

infix 8 range as ..

-- some :: forall f a. Alternative f => Lazy (f (OSet a)) => f a -> f (OSet a)
-- some x = OSet <$> (A.some x)

-- many :: forall f a. Alternative f => Lazy (f (OSet a)) => f a -> f (OSet a)
-- many x = OSet <$> (A.many x)

null :: forall a. OSet a -> Boolean
null (OSet xs) = A.null xs

length :: forall a. OSet a -> Int
length (OSet xs) = A.length xs

cons :: forall a. Eq a => a -> OSet a -> OSet a
cons x (OSet xs) = if A.elem x xs then OSet xs else OSet $ A.cons x xs

infixr 6 cons as :

snoc :: forall a. Eq a => OSet a -> a -> OSet a
snoc (OSet xs) x = if A.elem x xs then OSet xs else OSet $ A.snoc xs x

insert :: forall a. Ord a => a -> OSet a -> OSet a
insert x (OSet xs) = if A.elem x xs then OSet xs else OSet $ A.insert x xs

insertBy :: forall a. Eq a => (a -> a -> Ordering) -> a -> OSet a -> OSet a
insertBy cmp x (OSet ys) = if A.elem x ys then OSet ys else OSet $ A.insertBy cmp x ys

head :: forall a. OSet a -> Maybe a
head (OSet xs) = A.head xs

last :: forall a. OSet a -> Maybe a
last (OSet xs) = A.last xs

tail :: forall a. OSet a -> Maybe (OSet a)
tail (OSet xs) = OSet <$> A.tail xs

init :: forall a. OSet a -> Maybe (OSet a)
init (OSet xs) = OSet <$> A.init xs

uncons :: forall a. OSet a -> Maybe { head :: a, tail :: OSet a }
uncons (OSet xs) = map f $ A.uncons xs
  where f { head, tail } = { head: head, tail: OSet tail }

unsnoc :: forall a. OSet a -> Maybe { init :: OSet a, last :: a }
unsnoc (OSet xs) = map f $ A.unsnoc xs
  where f { init, last } = { init: OSet init, last: last}

index :: forall a. OSet a -> Int -> Maybe a
index (OSet xs) b = A.index xs b

infixl 8 index as !!

elemIndex :: forall a. Eq a => a -> OSet a -> Maybe Int
elemIndex a (OSet xs) = A.elemIndex a xs

elemLastIndex :: forall a. Eq a => a -> OSet a -> Maybe Int
elemLastIndex a (OSet xs) = A.elemLastIndex a xs

findIndex :: forall a. (a -> Boolean) -> OSet a -> Maybe Int
findIndex cmp (OSet xs) = A.findIndex cmp xs

findLastIndex :: forall a. (a -> Boolean) -> OSet a -> Maybe Int
findLastIndex cmp (OSet xs) = A.findLastIndex cmp xs

insertAt :: forall a. Eq a => Int -> a -> OSet a -> Maybe (OSet a)
insertAt p x (OSet xs) = map f $ A.insertAt p x xs
  where f xs' = if A.elem x xs then OSet xs else OSet xs'

deleteAt :: forall a. Int -> OSet a -> Maybe (OSet a)
deleteAt p (OSet xs) = OSet <$> A.deleteAt p xs

updateAt :: forall a. Eq a => Int -> a -> OSet a -> Maybe (OSet a)
updateAt p x (OSet xs) = map f $ A.updateAt p x xs
  where f xs' = if A.elem x xs then OSet xs else OSet xs'

updateAtIndices :: forall t a. Eq a => Foldable t => t (Tuple Int a) -> OSet a -> OSet a
updateAtIndices us (OSet xs) = OSet $ A.nubEq $ A.updateAtIndices us xs

modifyAt :: forall a. Eq a => Int -> (a -> a) -> OSet a -> Maybe (OSet a)
modifyAt p f (OSet xs) = map (OSet <<< A.nubEq) $ A.modifyAt p f xs

alterAt :: forall a. Eq a => Int -> (a -> Maybe a) -> OSet a -> Maybe (OSet a)
alterAt p f (OSet xs) = map (OSet <<< A.nubEq) $ A.alterAt p f xs

reverse :: forall a. OSet a -> OSet a
reverse (OSet xs) = OSet $ A.reverse xs

concat :: forall a. Eq a => OSet (OSet a) -> OSet a
concat (OSet xs) = OSet $ A.nubEq $ A.concat $ map (\(OSet xs') -> xs') xs

-- concatMap :: forall a b. (a -> OSet b) -> OSet a -> OSet b
-- concatMap f (OSet xs) = OSet $ A.concatMap f xs

filter :: forall a. (a -> Boolean) -> OSet a -> OSet a
filter f (OSet xs) = OSet $ A.filter f xs

partition :: forall a. (a -> Boolean) -> OSet a -> { no :: OSet a, yes :: OSet a }
partition f (OSet xs) = g $ A.partition f xs
  where g {no, yes} = {no: OSet no, yes: OSet yes}

filterA :: forall a f. Applicative f => (a -> f Boolean) -> OSet a -> f (OSet a)
filterA f (OSet xs) = OSet <$> A.filterA f xs

mapMaybe :: forall a b. (a -> Maybe b) -> OSet a -> OSet b
mapMaybe f (OSet xs) = OSet $ A.mapMaybe f xs

catMaybes :: forall a. OSet (Maybe a) -> OSet a
catMaybes (OSet xs) = OSet $ A.catMaybes xs

mapWithIndex :: forall a b. (Int -> a -> b) -> OSet a -> OSet b
mapWithIndex f (OSet xs) = OSet $ A.mapWithIndex f xs

sort :: forall a. Ord a => OSet a -> OSet a
sort (OSet xs) = OSet $ A.sort xs

sortBy :: forall a. (a -> a -> Ordering) -> OSet a -> OSet a
sortBy f (OSet xs) = OSet $ A.sortBy f xs

sortWith :: forall a b. Ord b => (a -> b) -> OSet a -> OSet a
sortWith f (OSet xs) = OSet $ A.sortWith f xs

slice :: forall a. Int -> Int -> OSet a -> OSet a
slice p q (OSet xs) = OSet $ A.slice p q xs

take :: forall a. Int -> OSet a -> OSet a
take p (OSet xs) = OSet $ A.take p xs

takeEnd :: forall a. Int -> OSet a -> OSet a
takeEnd p (OSet xs) = OSet $ A.takeEnd p xs

takeWhile :: forall a. (a -> Boolean) -> OSet a -> OSet a
takeWhile f (OSet xs) = OSet $ A.takeWhile f xs

drop :: forall a. Int -> OSet a -> OSet a
drop p (OSet xs) = OSet $ A.drop p xs

dropEnd :: forall a. Int -> OSet a -> OSet a
dropEnd p (OSet xs) = OSet $ A.dropEnd p xs

dropWhile :: forall a. (a -> Boolean) -> OSet a -> OSet a
dropWhile f (OSet xs) = OSet $ A.dropWhile f xs

span :: forall a. (a -> Boolean) -> OSet a -> { init :: OSet a, rest :: OSet a }
span f (OSet xs) = g $ A.span f xs
  where g {init, rest} = {init: OSet init, rest: OSet rest}

-- group :: forall a. Eq a => OSet a -> OSet (NonEmpty OSet a)
-- group (OSet xs) = OSet $ OSet <$> A.group xs

-- group' :: forall a. Ord a => OSet a -> OSet (NonEmpty OSet a)
-- group' (OSet xs) = OSet $ OSet <$> A.group' xs

-- groupBy :: forall a. (a -> a -> Boolean) -> OSet a -> OSet (NonEmptyArray a)
-- groupBy f (OSet xs) = OSet $ OSet <$> (A.groupBy f xs)

union :: forall a. Eq a => OSet a -> OSet a -> OSet a
union (OSet xs) (OSet ys) = OSet $ A.nubEq $ A.union xs ys

unionBy :: forall a. Eq a => (a -> a -> Boolean) -> OSet a -> OSet a -> OSet a
unionBy f (OSet xs) (OSet ys) = OSet $ A.nubEq $ A.unionBy f xs ys

delete :: forall a. Eq a => a -> OSet a -> OSet a
delete x (OSet ys) = OSet $ A.delete x ys

deleteBy :: forall a. (a -> a -> Boolean) -> a -> OSet a -> OSet a
deleteBy f x (OSet ys) = OSet $ A.deleteBy f x ys

difference :: forall a. Eq a => OSet a -> OSet a -> OSet a
difference (OSet xs) (OSet ys) = OSet $ A.difference xs ys

infix 5 difference as \\

intersect :: forall a. Eq a => OSet a -> OSet a -> OSet a
intersect (OSet xs) (OSet ys) = OSet $ A.intersect xs ys

intersectBy :: forall a. (a -> a -> Boolean) -> OSet a -> OSet a -> OSet a
intersectBy f (OSet xs) (OSet ys) = OSet $ A.intersectBy f xs ys

zipWith :: forall a b c. Eq c => (a -> b -> c) -> OSet a -> OSet b -> OSet c
zipWith f (OSet xs) (OSet ys) = OSet $ A.nubEq $ A.zipWith f xs ys

zipWithA :: forall m a b c. Eq c => Applicative m => (a -> b -> m c) -> OSet a -> OSet b -> m (OSet c)
zipWithA f (OSet xs) (OSet ys) = (OSet <<< A.nubEq) <$> A.zipWithA f xs ys

zip :: forall a b. OSet a -> OSet b -> OSet (Tuple a b)
zip (OSet xs) (OSet ys) = OSet $ A.zip xs ys

-- Warning: unlike Data.Array.unzip this function does not garantee a Tuple with OSet's of equal size
unzip :: forall a b. Eq a => Eq b => OSet (Tuple a b) -> Tuple (OSet a) (OSet b)
unzip (OSet xs) = f $ A.unzip xs
  where f (Tuple xs' ys') = Tuple (OSet $ A.nubEq xs') (OSet $ A.nubEq ys')

foldM :: forall m a b. Monad m => (a -> b -> m a) -> a -> OSet b -> m a
foldM f x (OSet ys) = A.foldM f x ys

foldRecM :: forall m a b. MonadRec m => (a -> b -> m a) -> a -> OSet b -> m a
foldRecM f x (OSet ys) = A.foldRecM f x ys

unsafeIndex :: forall a. Partial => OSet a -> Int -> a
unsafeIndex (OSet xs) p = A.unsafeIndex xs p
