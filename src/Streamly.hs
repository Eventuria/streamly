-- |
-- Module      : Streamly
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- The way a list represents a sequence of pure values, a stream represents a
-- sequence of monadic actions. The monadic stream API offered by Streamly is
-- very close to the Haskell "Prelude" pure lists' API, it can be considered as
-- a natural extension of lists to monadic actions. Streamly streams provide
-- concurrent composition and merging of streams. It can be considered as a
-- concurrent list transformer. In contrast to the "Prelude" lists, merging or
-- appending streams of arbitrary length is scalable and inexpensive.

{-# LANGUAGE CPP                       #-}
{-# LANGUAGE MultiParamTypeClasses                       #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

#include "Streamly/Streams/inline.hs"

module Streamly
    (
    -- * Composition Overview
    -- $overview

    -- * Streams Overview
    -- $streams

    -- * Type Synonyms
      MonadAsync

    -- * Stream transformers
    -- ** Serial Streams
    -- $serial
    , SerialT
    , WSerialT

    -- ** Concurrent Lookahead Streams
    -- $lookahead
    , AheadT

    -- ** Concurrent Asynchronous Streams
    -- $async
    , AsyncT
    , WAsyncT
    , ParallelT

    -- ** Zipping Streams
    -- $zipping
    , ZipSerialM
    , ZipAsyncM

    -- XXX stop exporting this from this module
    -- * Running Streams
    , P.runStream
    -- * Parallel Function Application
    -- $application
    , (|$)
    , (|&)
    , (|$.)
    , (|&.)
    , mkAsync

    -- * Merging Streams
    -- $sum
    , serial
    , wSerial
    , ahead
    , async
    , wAsync
    , parallel

    -- * Concurrency Control
    -- $concurrency
    , maxThreads
    , maxBuffer

    -- * Rate Limiting
    , Rate (..)
    , rate
    , avgRate
    , minRate
    , maxRate
    , constRate

    -- XXX stop exporting these from this module
    -- * Folding Containers of Streams
    -- $foldutils
    , foldWith
    , foldMapWith
    , forEachWith

    -- * Stream Type Adapters
    -- $adapters
    , IsStream ()

    , serially
    , wSerially
    , asyncly
    , aheadly
    , wAsyncly
    , parallely
    , zipSerially
    , zipAsyncly
    , adapt

    -- * IO Streams
    , Serial
    , WSerial
    , Ahead
    , Async
    , WAsync
    , Parallel
    , ZipSerial
    , ZipAsync

    -- * Re-exports
    , Semigroup (..)

    -- * Deprecated
    , Streaming
    , runStreaming
    , runStreamT
    , runInterleavedT
    , runAsyncT
    , runParallelT
    , runZipStream
    , runZipAsync
    , StreamT
    , InterleavedT
    , ZipStream
    , interleaving
    , zipping
    , zippingAsync
    , (<=>)
    , (<|)
    )
where

import Data.Semigroup (Semigroup(..))
import Streamly.SVar (MonadAsync, Rate(..))
import Streamly.Streams.Ahead
import Streamly.Streams.Async
import Streamly.Streams.Combinators
import Streamly.Streams.Parallel
import Streamly.Streams.Prelude
import Streamly.Streams.Serial
import Streamly.Streams.StreamK hiding (runStream, serial)
import Streamly.Streams.Zip

import qualified Streamly.Prelude as P
import qualified Streamly.Streams.StreamK as K

import Control.Monad.Trans.Control
import Control.Monad.Base
import Data.Functor.Identity
import Control.Monad.IO.Class

-- $streams
-- The basic stream type is 'Serial', it represents a sequence of IO actions,
-- and is a 'Monad'.  The type 'SerialT' is a monad transformer that can
-- represent a sequence of actions in an arbitrary monad. The type 'Serial' is
-- in fact a synonym for @SerialT IO@.  There are a few more types similar to
-- 'SerialT', all of them represent a stream and differ only in the
-- 'Semigroup', 'Applicative' and 'Monad' compositions of the stream. 'Serial'
-- and 'WSerial' types compose serially whereas 'Async' and 'WAsync'
-- types compose concurrently. All these types can be freely inter-converted
-- using type combinators without any cost. You can freely switch to any type
-- of composition at any point in the program.  When no type annotation or
-- explicit stream type combinators are used, the default stream type is
-- inferred as 'Serial'.
--
-- Here is a simple console echo program example:
--
-- @
-- > runStream $ S.repeatM getLine & S.mapM putStrLn
-- @
--
-- For more details please see the "Streamly.Tutorial" module and the examples
-- directory in this package.
--
-- This module exports stream types, instances and some basic operations.
-- Functionality exported by this module include:
--
-- * Semigroup append ('<>') instances as well as explicit  operations for merging streams
-- * Monad and Applicative instances for looping over streams
-- * Zip Applicatives for zipping streams
-- * Stream type combinators to convert between different composition styles
-- * Some basic utilities to run and fold streams
--
-- See the "Streamly.Prelude" module for comprehensive APIs for construction,
-- generation, elimination and transformation of streams.
--
-- This module is designed to be imported unqualified:
--
-- @
-- import Streamly
-- @

-- $overview
-- Let us provide a quick overview of streaming composition using streamly. The
-- stream types 'SerialT' etc exported from this module are producers of
-- streams. A stream can be transformed in many ways e.g. map, scan, filter
-- etc.  Scans can perform stateful transformations.  Multiple transformations
-- can be chained into a pipeline.  Finally, the stream can be folded into a
-- result or folded to generate effects. See "Streamly.Prelude" module for
-- more details.
--
-- @
--
-- ---Stream m a---transforms(map, scan, filter)---Stream m b----Fold m b c
-- @
--
-- Multiple streams can be composed in many ways e.g. zipping, merging,
-- appending, monadic nesting etc to build a composed stream producer. See
-- "Streamly.Prelude" module for more details.
--
-- @
--
-- -------Stream m a---transform---|
--                                 |
-- -------Stream m a---transform---|=>---transform---Stream m a--->
--                                 |
-- -------Stream m a---transform---|
-- @
--
-- Like Stream is a producer, a Fold is a consumer of streams. The way multiple
-- producers can be combined into a single producer, we can also combine
-- multiple consumers (i.e. folds) into a single consumer. The input to the
-- combined fold can be distributed over the constituents of the composed fold
-- in different ways.  For example, the same input can be fed to all of them,
-- or we can demultiplex the input to different folds based on some criterion.
-- In other words, a stream can be cloned or split into multiple streams and
-- then transformed and combined into a single result. Folds can be transformed
-- contravariantly i.e. we can map or filter the inputs of individual folds.
-- See the "Streamly.Foldl" module for more details.
--
-- @
--
--                             |---transform----Foldl m a b--------|
-- ---stream m a---transform---|                                   |---f b c ...
--                             |---transform----Foldl m a c--------|
--                             |                                   |
--                                        ...
-- @
--
-- Folds and Sinks are consumers of streams. Folds and Sinks can be composed
-- comonadically such that the same input can be shared across multiple folds
-- or demultiplexed into different folds and the outputs of the folds can be
-- combined in a desired manner. Fold inputs can be transformed before it is
-- fed to the fold:
--

-- XXX This should perhaps be moved to Prelude.

------------------------------------------------------------------------------
-- Eliminating a stream
------------------------------------------------------------------------------

-- | Same as 'runStream'
--
-- @since 0.1.0
{-# DEPRECATED runStreaming "Please use runStream instead." #-}
runStreaming :: (Monad m, IsStream t) => t m a -> m ()
runStreaming = P.runStream . K.adapt

-- | Same as @runStream@.
--
-- @since 0.1.0
{-# DEPRECATED runStreamT "Please use runStream instead." #-}
runStreamT :: Monad m => SerialT m a -> m ()
runStreamT = P.runStream

-- | Same as @runStream . wSerially@.
--
-- @since 0.1.0
{-# DEPRECATED runInterleavedT "Please use 'runStream . interleaving' instead." #-}
runInterleavedT :: Monad m => WSerialT m a -> m ()
runInterleavedT = P.runStream . K.adapt

-- | Same as @runStream . parallely@.
--
-- @since 0.1.0
{-# DEPRECATED runParallelT "Please use 'runStream . parallely' instead." #-}
runParallelT :: Monad m => ParallelT m a -> m ()
runParallelT = P.runStream . K.adapt

-- | Same as @runStream . asyncly@.
--
-- @since 0.1.0
{-# DEPRECATED runAsyncT "Please use 'runStream . asyncly' instead." #-}
runAsyncT :: Monad m => AsyncT m a -> m ()
runAsyncT = P.runStream . K.adapt

-- | Same as @runStream . zipping@.
--
-- @since 0.1.0
{-# DEPRECATED runZipStream "Please use 'runStream . zipSerially instead." #-}
runZipStream :: Monad m => ZipSerialM m a -> m ()
runZipStream = P.runStream . K.adapt

-- | Same as @runStream . zippingAsync@.
--
-- @since 0.1.0
{-# DEPRECATED runZipAsync "Please use 'runStream . zipAsyncly instead." #-}
runZipAsync :: Monad m => ZipAsyncM m a -> m ()
runZipAsync = P.runStream . K.adapt

------------------------------------------------------------------------------
-- Documentation
------------------------------------------------------------------------------

-- $serial
--
-- Serial streams compose serially or non-concurrently. In a composed stream,
-- each action is executed only after the previous action has finished.  The two
-- serial stream types 'SerialT' and 'WSerialT' differ in how they traverse the
-- streams in a 'Semigroup' or 'Monad' composition.

-- $async
--
-- The async style streams execute actions asynchronously and consume the
-- outputs as well asynchronously. In a composed stream, at any point of time
-- more than one stream can run concurrently and yield elements.  The elements
-- are yielded by the composed stream as they are generated by the constituent
-- streams on a first come first serve basis.  Therefore, on each run the
-- stream may yield elements in a different sequence depending on the delays
-- introduced by scheduling.  The two async types 'AsyncT' and 'WAsyncT' differ
-- in how they traverse streams in 'Semigroup' or 'Monad' compositions.

-- $zipping
--
-- 'ZipSerialM' and 'ZipAsyncM', provide 'Applicative' instances for zipping the
-- corresponding elements of two streams together. Note that these types are
-- not monads.

-- $application
--
-- Stream processing functions can be composed in a chain using function
-- application with or without the '$' operator, or with reverse function
-- application operator '&'. Streamly provides concurrent versions of these
-- operators applying stream processing functions such that each stage of the
-- stream can run in parallel. The operators start with a @|@; we can read '|$'
-- as "@parallel dollar@" to remember that @|@ comes before '$'.
--
-- Imports for the code snippets below:
--
-- @
--  import Streamly
--  import qualified Streamly.Prelude as S
--  import Control.Concurrent
-- @

-- $sum
-- The 'Semigroup' operation '<>' of each stream type combines two streams in a
-- type specific manner. This section provides polymorphic versions of '<>'
-- which can be used to combine two streams in a predetermined way irrespective
-- of the type.

-- XXX An alternative design choice would be to let a control parameter affect
-- the nearest SVar only and then it gets cleared. The benefit of the current
-- choice is that it is simply just like global configuration, just like state
-- behaves, so should be easy to comprehend. But it has the downside of leaking
-- to undesired actions, that is we can forget to reset it.
--
-- $concurrency
--
-- These combinators can be used at any point in a stream composition to set
-- parameters to control the concurrency of the enclosed stream.  A parameter
-- set at any point remains effective for any concurrent combinators used
-- downstream until it is reset.  These control parameters have no effect on
-- non-concurrent combinators in the stream, or on non-concurrent streams. They
-- also do not affect 'Parallel' streams, as concurrency for 'Parallel' streams
-- is always unbounded.

-- $adapters
--
-- You may want to use different stream composition styles at different points
-- in your program. Stream types can be freely converted or adapted from one
-- type to another.  The 'IsStream' type class facilitates type conversion of
-- one stream type to another. It is not used directly, instead the type
-- combinators provided below are used for conversions.
--
-- To adapt from one monomorphic type (e.g. 'AsyncT') to another monomorphic
-- type (e.g. 'SerialT') use the 'adapt' combinator. To give a polymorphic code
-- a specific interpretation or to adapt a specific type to a polymorphic type
-- use the type specific combinators e.g. 'asyncly' or 'wSerially'. You
-- cannot adapt polymorphic code to polymorphic code, as the compiler would not know
-- which specific type you are converting from or to. If you see a an
-- @ambiguous type variable@ error then most likely you are using 'adapt'
-- unnecessarily on polymorphic code.
--

-- $foldutils
--
-- These are variants of standard 'Foldable' fold functions that use a
-- polymorphic stream sum operation (e.g. 'async' or 'wSerial') to fold a
-- container of streams.
