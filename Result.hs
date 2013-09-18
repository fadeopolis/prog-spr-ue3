{-# LANGUAGE FlexibleInstances #-}

module Result(
	Result(..),

	resultToList,
--	fromMaybe,
	catch
) where

import Control.Monad
import Control.Monad.Error
import Control.Applicative
import Data.Monoid

-- | Return value for functions that may fail
data Result a = Ok  a
              | Err String
	deriving (Show, Eq)

resultToList (Ok a) = [a]
resultToList _      = []

--fromMaybe (Just a) = Ok  a
--fromMaybe Nothing  = Err "Nothing"

catch (Ok a) msg = Ok  a
catch _      msg = Err msg

instance Monad Result where
	return = Ok
	fail   = Err

	(Ok  a) >>= f = f a
	(Err e) >>= f = Err e

instance MonadPlus Result where
	mzero = fail "mzero"

	mplus (Ok a) _      = Ok a
	mplus _      (Ok a) = Ok a
	mplus a      _      = a

instance Functor Result where
	fmap = liftM

instance Applicative Result where
	pure  = return
	(<*>) = ap
