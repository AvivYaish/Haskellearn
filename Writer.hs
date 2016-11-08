-- A module of a writer, which is capable of chaining logs and values in a "monadic" way.

module Writer (Writer(..), DiffList(..), tell, toDiffList, fromDiffList,
  getValFromWriter, showIndent, takeFromDiffList) where

import Control.Monad (liftM, ap)
import Data.String.Utils

-- Writer data type represents a log writer
newtype Writer w a = Writer { runWriter :: (a, w) }

-- DiffList data type represents a difference list.
-- For more information:
-- https://wiki.haskell.org/Difference_list
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

-- Every Monad is an Applicative, so that means Monads are also Functors.
-- liftM is fmap implemented with return and (>>=), and thus allows us to
-- expose the Functor within the Monad
instance (Monoid w) => Functor (Writer w) where
  fmap = liftM

-- Since GHC 7.10 Applicative was defined as a superclass of Monad -------------
instance (Monoid w) =>  Applicative (Writer w) where
  pure  = return
  (<*>) = ap
--------------------------------------------------------------------------------

-- return should wrap a value x with our Writer monad
-- (Writer (x, v)) >>= f should activate f on x, getting a value y, append the
-- new log v' to v, and return Writer (y, v 'mappend' v')
instance (Monoid w) => Monad (Writer w) where
    return x = Writer (x, mempty)
    (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)
    (DiffList f) `mappend` (DiffList g) = DiffList (f.g)

-- A function that is used to create a writer with "dummy" values so a simple text
-- will be written to the log.
tell :: w -> Writer w ()
tell x = Writer ((), x)

-- A function that is used to get the result value from the writer.
getValFromWriter :: Writer w a -> a
getValFromWriter (Writer (a, _)) = a

-- A function that is used to get a difference list from a given standard list
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

-- A function that is used to get a standard list from a given diffrent list.
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- Adds indentation to the Show instance of the given object
-- useful to make the logs prettier
showIndent :: (Show a) => a -> String
showIndent obj = "\t" ++ replace "\n" "\n\t" (show obj)

-- Takes the n first items from a DiffList.
takeFromDiffList :: Int -> DiffList a -> [a]
takeFromDiffList n diffList = take n (fromDiffList diffList)
