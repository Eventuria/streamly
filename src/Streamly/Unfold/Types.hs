{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification          #-}
{-# LANGUAGE FlexibleContexts                   #-}

-- |
-- Module      : Streamly.Unfold.Types
-- Copyright   : (c) 2019 Composewell Technologies
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Streamly.Unfold.Types
    ( Unfold (..)
    )
where

import Streamly.Streams.StreamD.Type (Step(..))

------------------------------------------------------------------------------
-- Monadic Unfolds
------------------------------------------------------------------------------

-- | An @Unfold m a b@ is a generator of a stream of values of type @b@ from a
-- seed of type 'a' in 'Monad' @m@.
--
-- @since 0.7.0

data Unfold m a b =
    -- | @Unfold step inject@
    forall s. Unfold (s -> m (Step s b)) (a -> m s)

{-
-- If we can extract the state then we can run the unfold one step at a time.
-- We can run a step and then extract the state. We can then re-inject the
-- state to resume the unfold. The remaining seed is the seed leftover.
--
-- Will it always be possible to get back a from s? It may not.
--
data UnfoldK m a b =
    -- | @Unfold step inject extract@
    forall s. Unfold (s -> m (Step s b)) (a -> m s) (s -> a)
-}
