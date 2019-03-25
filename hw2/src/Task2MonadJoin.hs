{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Task2MonadJoin where

import Task2Monad (Monad (..), MonadJoin (..))

instance Monad m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = return
  join :: m (m a) -> m a
  join x = x >>= (\y -> y)
