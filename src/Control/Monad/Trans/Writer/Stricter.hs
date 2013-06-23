{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fexpose-all-unfoldings #-}
module Control.Monad.Trans.Writer.Stricter
       ( WriterT (), writerT, runWriterT
       , Writer, runWriter
       , execWriterT, mapWriterT
       , module Control.Monad.Writer.Class
#ifdef ENABLE_TESTS
       , test
#endif
       ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Writer.Class
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid

#ifdef ENABLE_TESTS
import Control.Arrow
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
#endif

type Writer w = WriterT w Identity

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

newtype WriterT w m a = WriterT { unWriterT :: StateT w m a }
                      deriving ( Functor, Applicative, Alternative, Monad
                               , MonadPlus, MonadFix, MonadIO, MonadTrans
                               )

writerT :: (Monoid w, Monad m) => m (a, w) -> WriterT w m a
writerT maw = WriterT . StateT $ \w -> do
  (a, w') <- maw
  return (a, w <> w')

runWriterT :: Monoid w => WriterT w m a -> m (a, w)
runWriterT = (`runStateT` mempty) . unWriterT

execWriterT :: (Monad m, Monoid w) => WriterT w m a -> m w
execWriterT = liftM snd . runWriterT

mapWriterT :: (Monad n, Monoid w, Monoid w') =>
              (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f = writerT . f . runWriterT

instance (Foldable f, Monoid w) => Foldable (WriterT w f) where
  foldMap f = foldMap (f . fst) . runWriterT
  
instance (Traversable f, Monad f, Monoid w) => Traversable (WriterT w f) where
  traverse f = fmap writerT . (traverse._1) f . runWriterT

instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
  writer = writerT . return
  tell = WriterT . modify . flip (<>)
  listen m = writerT $ liftM (\(a, w) -> ((a, w), w)) (runWriterT m)
  pass m = writerT $ liftM (\((a, f), w) -> (a, f w)) (runWriterT m)

#ifdef ENABLE_TESTS
data TestBase a = Return a
                | Roll (TestBase a)
                deriving (Eq, Ord, Read, Show)

instance Monad TestBase where
  return = Return
  Return x >>= f = f x
  Roll m >>= f = Roll $ m >>= f

instance Arbitrary a => Arbitrary (TestBase a) where
  arbitrary = do
    done <- arbitrary
    if done then Return <$> arbitrary else Roll <$> arbitrary

prop_writerRunWriter :: (Int, Int) -> Bool
prop_writerRunWriter (second Sum -> x) = runWriter (writer x) == Strict.runWriter (writer x)

prop_writerTRunWriterT :: TestBase (Int, Int) -> Bool
prop_writerTRunWriterT ((liftM.second) Sum -> x) = runWriterT (writerT x) == Strict.runWriterT (Strict.WriterT x)

prop_return :: Int -> Bool
prop_return x = runWriterT (return x :: WriterT (Sum Int) TestBase Int) == Strict.runWriterT (return x)

prop_bind :: TestBase (Int, Int) -> Fun Int (TestBase (Int, Int)) -> Bool
prop_bind ((liftM.second) Sum -> m) ((fmap.liftM.second) Sum . apply -> f) =
  runWriterT (writerT m >>= writerT . f) == Strict.runWriterT (Strict.WriterT m >>= Strict.WriterT . f)

prop_tell :: Int -> Bool
prop_tell (Sum -> x) = runWriterT (tell x :: WriterT (Sum Int) TestBase ()) == Strict.runWriterT (tell x)

prop_listen :: TestBase (Int, Int) -> Bool
prop_listen ((liftM.second) Sum -> x) =
  runWriterT (listen (writerT x)) == Strict.runWriterT (listen (Strict.WriterT x))

prop_pass :: TestBase ((Int, Fun Int Int), Int) -> Bool
prop_pass (liftM (second ((Sum .) . (. getSum) . apply) *** Sum) -> x) =
  runWriterT (pass (writerT x)) == Strict.runWriterT (pass (Strict.WriterT x))

prop_empty :: Bool
prop_empty = runWriterT (empty :: WriterT (Sum Int) [] Int) == Strict.runWriterT empty

prop_orElse :: [(Int, Int)] -> [(Int, Int)] -> Bool
prop_orElse ((map.second) Sum -> x) ((map.second) Sum -> y) =
  runWriterT (writerT x <|> writerT y) == Strict.runWriterT (Strict.WriterT x <|> Strict.WriterT y)

test :: Test
test = testGroup "Control.Monad.Trans.Writer.Stricter"
       [ testProperty "runWriter . writer" prop_writerRunWriter
       , testProperty "runWriterT . writerT" prop_writerTRunWriterT
       , testProperty "return" prop_return
       , testProperty "(>>=)" prop_bind
       , testProperty "tell" prop_tell
       , testProperty "listen" prop_listen
       , testProperty "pass" prop_pass
       , testProperty "empty" prop_empty
       , testProperty "(<|>)" prop_orElse
       ]
#endif
