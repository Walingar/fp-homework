{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Task2MonadFish where

import Task2Monad (Monad (..), MonadFish (..), MonadJoin (..))

instance Monad m => MonadFish m where
  returnFish :: a -> m a
  returnFish = return
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  (>=>) f g x = f x >>= g

instance MonadFish m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = returnFish
  join :: m (m a) -> m a
  join = (\y -> y) >=> (\y -> y)
