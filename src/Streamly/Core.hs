{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-} -- XXX

-- |
-- Module      : Streamly.Core
-- Copyright   : (c) 2017 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
--
module Streamly.Core
    (
      MonadAsync

    -- * Streams
    , Stream (..)

    -- * Construction (pure)
    , nil
    , cons
    , singleton
    , once
    , repeat

    -- * Construction (monadic)
    , consM
    , consMAhead
    , consMAsync
    , consMWAsync
    , consMParallel

    -- * Semigroup Style Composition
    , serial
    , wSerial
    , ahead
    , async
    , wAsync
    , parallel

    -- * applications
    , applyWith
    , runWith

    -- * zip
    , zipWith
    , zipAsyncWith

    -- * Concurrent Stream Vars (SVars)
    , SVar
    , SVarStyle (..)
    , newStreamVar1
    , fromStreamVar
    , toStreamVar
    )
where

import           Control.Concurrent          (ThreadId, myThreadId, threadDelay)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, tryTakeMVar,
                                              tryPutMVar, takeMVar)
import           Control.Exception           (SomeException (..), catch, mask)
import           Control.Monad               (when)
import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class      (MonadIO(..))
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Control (MonadBaseControl, control)
import           Data.Atomics                (casIORef, readForCAS, peekTicket
                                             ,atomicModifyIORefCAS_
                                             ,writeBarrier)
import           Data.Concurrent.Queue.MichaelScott (LinkedQueue, newQ, pushL,
                                                     tryPopR, nullQ)
import           Data.Functor                (void)
import           Data.Heap                   (Heap, Entry(..))
import qualified Data.Heap                   as H
import           Data.IORef                  (IORef, modifyIORef, newIORef,
                                              readIORef, atomicModifyIORef
#ifdef DIAGNOSTICS
                                              , writeIORef
#endif
                                              )
import           Data.Maybe                  (isNothing, fromJust)
import           Data.Semigroup              (Semigroup(..))
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Prelude                     hiding (repeat, zipWith)

import GHC.Exts
import GHC.Conc (ThreadId(..))
import GHC.IO (IO(..))

-- MVar diagnostics has some overhead - around 5% on asyncly null benchmark, we
-- can keep it on in production to debug problems quickly if and when they
-- happen, but it may result in unexpected output when threads are left hanging
-- until they are GCed because the consumer went away.

#ifdef DIAGNOSTICS
import           Control.Exception           (catches, throwIO, Handler(..),
                                              BlockedIndefinitelyOnMVar(..),
                                              BlockedIndefinitelyOnSTM(..))
import System.IO (hPutStrLn, stderr)
#endif

------------------------------------------------------------------------------
-- Parent child thread communication type
------------------------------------------------------------------------------

-- | Events that a child thread may send to a parent thread.
data ChildEvent a =
      ChildYield a
    | ChildStop ThreadId (Maybe SomeException)

-- | Sorting out-of-turn outputs in a heap for Ahead style streams
data AheadHeapEntry m a =
      AheadEntryPure a
    | AheadEntryStream (Stream m a)

------------------------------------------------------------------------------
-- State threaded around the monad for thread management
------------------------------------------------------------------------------

-- XXX use a separate data structure for each type of SVar
-- | Identify the type of the SVar. Two computations using the same style can
-- be scheduled on the same SVar.
data SVarStyle =
      AsyncVar             -- depth first concurrent
    | WAsyncVar            -- breadth first concurrent
    | ParallelVar          -- all parallel
    | AheadVar             -- Concurrent look ahead
    deriving (Eq, Show)

-- | An SVar or a Stream Var is a conduit to the output from multiple streams
-- running concurrently and asynchronously. An SVar can be thought of as an
-- asynchronous IO handle. We can write any number of streams to an SVar in a
-- non-blocking manner and then read them back at any time at any pace.  The
-- SVar would run the streams asynchronously and accumulate results. An SVar
-- may not really execute the stream completely and accumulate all the results.
-- However, it ensures that the reader can read the results at whatever paces
-- it wants to read. The SVar monitors and adapts to the consumer's pace.
--
-- An SVar is a mini scheduler, it has an associated runqueue that holds the
-- stream tasks to be picked and run by a pool of worker threads. It has an
-- associated output queue where the output stream elements are placed by the
-- worker threads. A doorBell is used by the worker threads to intimate the
-- consumer thread about availability of new results in the output queue. More
-- workers are added to the SVar by 'fromStreamVar' on demand if the output
-- produced is not keeping pace with the consumer. On bounded SVars, workers
-- block on the output queue to provide throttling of the producer  when the
-- consumer is not pulling fast enough.  The number of workers may even get
-- reduced depending on the consuming pace.
--
-- New work is enqueued either at the time of creation of the SVar or as a
-- result of executing the parallel combinators i.e. '<|' and '<|>' when the
-- already enqueued computations get evaluated. See 'joinStreamVarAsync'.
--
data SVar m a =
       SVar {
            -- Read only state
              svarStyle      :: SVarStyle

            -- Shared output queue (events, length)
            , outputQueue    :: IORef ([ChildEvent a], Int)
            , doorBell       :: MVar ()  -- signal the consumer about output

            -- Output synchronization mechanism for Ahead streams (Ahead and
            -- wAhead). We maintain a heap of out of sequence ahead of time
            -- generated outputs and the sequence number of the task that is
            -- currently at the head of the stream.  Concurrent execute ahead
            -- tasks that have a sequence number greater than the task at the
            -- head should add their output to the heap.
            , outputHeap    :: IORef (Heap (Entry Int (AheadHeapEntry m a))
                                     , Int
                                     )

            -- Shared work queue
            , workQueue      :: IORef ([Stream m a], Int)
            , enqueue        :: Stream m a -> IO ()
            , queueEmpty     :: m Bool
            , waitingForWork :: IORef Bool
            , runqueue       :: m ()

            -- Shared, thread tracking
            , runningThreads :: IORef (Set ThreadId)
            , activeWorkers  :: IORef Int
#ifdef DIAGNOSTICS
            , totalDispatches :: IORef Int
            , maxWorkers   :: IORef Int
            , maxOutQSize  :: IORef Int
            , maxHeapSize  :: IORef Int
            , maxWorkQSize :: IORef Int
#endif
            }

#ifdef DIAGNOSTICS
{-# NOINLINE dumpSVar #-}
dumpSVar :: SVar m a -> IO String
dumpSVar sv = do
    tid <- myThreadId
    (oqList, oqLen) <- readIORef $ outputQueue sv
    db <- tryTakeMVar $ doorBell sv
    aheadDump <-
        if svarStyle sv == AheadVar
        then do
            (oheap, oheapSeq) <- readIORef $ outputHeap sv
            (wq, wqSeq) <- readIORef $ workQueue sv
            maxHp <- readIORef $ maxHeapSize sv
            return $ unlines
                [ "heap length = " ++ show (H.size oheap)
                , "heap seqeunce = " ++ show oheapSeq
                , "work queue length = " ++ show (length wq)
                , "work queue sequence = " ++ show wqSeq
                , "heap max size = " ++ show maxHp
                ]
        else return []

    waiting <- readIORef $ waitingForWork sv
    rthread <- readIORef $ runningThreads sv
    workers <- readIORef $ activeWorkers sv
    maxWrk <- readIORef $ maxWorkers sv
    dispatches <- readIORef $ totalDispatches sv
    maxOq <- readIORef $ maxOutQSize sv
    -- XXX queueEmpty should be made IO return type

    return $ unlines
        [ "tid = " ++ show tid
        , "style = " ++ show (svarStyle sv)
        , "outputQueue length computed  = " ++ show (length oqList)
        , "outputQueue length maintained = " ++ show oqLen
        , "output doorBell = " ++ show db
        , "total dispatches = " ++ show dispatches
        , "max workers = " ++ show maxWrk
        , "max outQSize = " ++ show maxOq
        ]
        ++ aheadDump ++ unlines
        [ "waitingForWork = " ++ show waiting
        , "running threads = " ++ show rthread
        , "running thread count = " ++ show workers
        ]

{-# NOINLINE mvarExcHandler #-}
mvarExcHandler :: SVar m a -> String -> BlockedIndefinitelyOnMVar -> IO ()
mvarExcHandler sv label e@BlockedIndefinitelyOnMVar = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ label ++ " " ++ "BlockedIndefinitelyOnMVar\n" ++ svInfo
    throwIO e

{-# NOINLINE stmExcHandler #-}
stmExcHandler :: SVar m a -> String -> BlockedIndefinitelyOnSTM -> IO ()
stmExcHandler sv label e@BlockedIndefinitelyOnSTM = do
    svInfo <- dumpSVar sv
    hPutStrLn stderr $ label ++ " " ++ "BlockedIndefinitelyOnSTM\n" ++ svInfo
    throwIO e

withDBGMVar :: SVar m a -> String -> IO () -> IO ()
withDBGMVar sv label action =
    action `catches` [ Handler (mvarExcHandler sv label)
                     , Handler (stmExcHandler sv label)
                     ]
#else
withDBGMVar :: SVar m a -> String -> IO () -> IO ()
withDBGMVar _ _ action = action
#endif

-- Slightly faster version of CAS. Gained some improvement by avoiding the use
-- of "evaluate" because we know we do not have exceptions in fn.
{-# INLINE atomicModifyIORefCAS #-}
atomicModifyIORefCAS :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORefCAS ref fn = do
    tkt <- readForCAS ref
    loop tkt retries

    where

    retries = 25 :: Int
    loop _   0     = atomicModifyIORef ref fn
    loop old tries = do
        let (new, result) = fn $ peekTicket old
        (success, tkt) <- casIORef ref old new
        if success
        then return result
        else loop tkt (tries - 1)

------------------------------------------------------------------------------
-- The stream type
------------------------------------------------------------------------------

-- | The type 'Stream m a' represents a monadic stream of values of type 'a'
-- constructed using actions in monad 'm'. It uses stop, singleton and yield
-- continuations equivalent to the following direct style type:
--
-- data Stream m a = Stop | Singleton a | Yield a (Stream m a)
--
-- To facilitate parallel composition we maintain a local state in an SVar that
-- is shared across and is used for synchronization of the streams being
-- composed.
--
-- The singleton case can be expressed in terms of stop and yield but we have
-- it as a separate case to optimize composition operations for streams with
-- single element.  We build singleton streams in the implementation of 'pure'
-- for Applicative and Monad, and in 'lift' for MonadTrans.
--
newtype Stream m a =
    Stream {
        runStream :: forall r.
               Maybe (SVar m a)          -- local state
            -> m r                       -- stop
            -> (a -> m r)                -- singleton
            -> (a -> Stream m a -> m r)  -- yield
            -> m r
    }

nil :: Stream m a
nil = Stream $ \_ stp _ _ -> stp

-- | faster than consM because there is no bind.
cons :: a -> Stream m a -> Stream m a
cons a r = Stream $ \_ _ _ yld -> yld a r

-- | Same as @once . return@ but may be faster because there is no bind
singleton :: a -> Stream m a
singleton a = Stream $ \_ _ single _ -> single a

{-# INLINE once #-}
once :: Monad m => m a -> Stream m a
once m = Stream $ \_ _ single _ -> m >>= single

{-# INLINE consM #-}
consM :: Monad m => m a -> Stream m a -> Stream m a
consM m r = Stream $ \_ _ _ yld -> m >>= \a -> yld a r

repeat :: a -> Stream m a
repeat a = let x = cons a x in x

------------------------------------------------------------------------------
-- Semigroup
------------------------------------------------------------------------------

-- | Concatenates two streams sequentially i.e. the first stream is
-- exhausted completely before yielding any element from the second stream.
{-# INLINE serial #-}
serial :: Stream m a -> Stream m a -> Stream m a
serial m1 m2 = go m1
    where
    go (Stream m) = Stream $ \_ stp sng yld ->
            let stop      = (runStream m2) Nothing stp sng yld
                single a  = yld a m2
                yield a r = yld a (go r)
            in m Nothing stop single yield

instance Semigroup (Stream m a) where
    (<>) = serial

------------------------------------------------------------------------------
-- Monoid
------------------------------------------------------------------------------

instance Monoid (Stream m a) where
    mempty = nil
    mappend = (<>)

------------------------------------------------------------------------------
-- Interleave
------------------------------------------------------------------------------

{-# INLINE wSerial #-}
wSerial :: Stream m a -> Stream m a -> Stream m a
wSerial m1 m2 = Stream $ \_ stp sng yld -> do
    let stop      = (runStream m2) Nothing stp sng yld
        single a  = yld a m2
        yield a r = yld a (wSerial m2 r)
    (runStream m1) Nothing stop single yield

------------------------------------------------------------------------------
-- Spawning threads and collecting result in streamed fashion
------------------------------------------------------------------------------

-- | A monad that can perform concurrent or parallel IO operations. Streams
-- that can be composed concurrently require the underlying monad to be
-- 'MonadAsync'.
--
-- @since 0.1.0
type MonadAsync m = (MonadIO m, MonadBaseControl IO m, MonadThrow m)

-- Stolen from the async package. The perf improvement is modest, 2% on a
-- thread heavy benchmark (parallel composition using noop computations).
-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)

{-# INLINE doFork #-}
doFork :: MonadBaseControl IO m
    => m ()
    -> (SomeException -> IO ())
    -> m ThreadId
doFork action exHandler =
    control $ \runInIO ->
        mask $ \restore -> do
                tid <- rawForkIO $ catch (restore $ void $ runInIO action)
                                         exHandler
                runInIO (return tid)

-- XXX exception safety of all atomic/MVar operations

-- TBD Each worker can have their own queue and the consumer can empty one
-- queue at a time, that way contention can be reduced.

-- | This function is used by the producer threads to queue output for the
-- consumer thread to consume. Returns whether the queue has more space.
{-# NOINLINE send #-}
send :: SVar m a -> ChildEvent a -> IO Bool
send sv msg = do
    len <- atomicModifyIORefCAS (outputQueue sv) $ \(es, n) ->
        ((msg : es, n + 1), n)
    when (len <= 0) $ do
        -- The wake up must happen only after the store has finished otherwise
        -- we can have lost wakeup problems.
        writeBarrier
        -- Since multiple workers can try this at the same time, it is possible
        -- that we may put a spurious MVar after the consumer has already seen
        -- the output. But that's harmless, at worst it may cause the consumer
        -- to read the queue again and find it empty.
        -- The important point is that the consumer is guaranteed to receive a
        -- doorbell if something was added to the queue after it empties it.
        void $ tryPutMVar (doorBell sv) ()
    return (len < 1500)

{-# NOINLINE sendStop #-}
sendStop :: SVar m a -> IO ()
sendStop sv = myThreadId >>= \tid -> void $ send sv (ChildStop tid Nothing)

-------------------------------------------------------------------------------
-- Async
-------------------------------------------------------------------------------

-- Note: For purely right associated expressions this queue should have at most
-- one element. It grows to more than one when we have left associcated
-- expressions. Large left associated compositions can grow this to a
-- large size
{-# INLINE enqueueLIFO #-}
enqueueLIFO :: SVar m a -> IORef [Stream m a] -> Stream m a -> IO ()
enqueueLIFO sv q m = do
    w <- readIORef $ waitingForWork sv
    if w
    then withDoorBell
    else atomicModifyIORefCAS_ q $ \ms -> m : ms

    where

    withDoorBell = do
        v <- atomicModifyIORefCAS q $ \ ms ->
            case ms of
                [] -> (m : ms, True)
                _ -> (m : ms, False)
        writeBarrier
        when v $ void $ tryPutMVar (doorBell sv) ()
        atomicModifyIORefCAS_ (waitingForWork sv) (const False)

runqueueLIFO :: MonadIO m => SVar m a -> IORef [Stream m a] -> m ()
runqueueLIFO sv q = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> liftIO $ sendStop sv
            Just m -> (runStream m) (Just sv) run single yield

    single a = do
        res <- liftIO $ send sv (ChildYield a)
        if res then run else liftIO $ sendStop sv
    yield a r = do
        res <- liftIO $ send sv (ChildYield a)
        if res
        then (runStream r) (Just sv) run single yield
        else liftIO $ enqueueLIFO sv q r >> sendStop sv

    dequeue = liftIO $ atomicModifyIORefCAS q $ \case
                [] -> ([], Nothing)
                x : xs -> (xs, Just x)

-------------------------------------------------------------------------------
-- WAsync
-------------------------------------------------------------------------------

{-# INLINE enqueueFIFO #-}
enqueueFIFO :: SVar m a -> LinkedQueue (Stream m a) -> Stream m a -> IO ()
enqueueFIFO sv q m = do
    w <- readIORef $ waitingForWork sv
    if w
    then withDoorBell
    else pushL q m

    where

    withDoorBell = do
        emp <- nullQ q
        pushL q m
        writeBarrier
        when emp $ void $ tryPutMVar (doorBell sv) ()
        atomicModifyIORefCAS_ (waitingForWork sv) (const False)

runqueueFIFO :: MonadIO m => SVar m a -> LinkedQueue (Stream m a) -> m ()
runqueueFIFO sv q = run

    where

    run = do
        work <- dequeue
        case work of
            Nothing -> liftIO $ sendStop sv
            Just m -> (runStream m) (Just sv) run single yield

    dequeue = liftIO $ tryPopR q
    single a = do
        res <- liftIO $ send sv (ChildYield a)
        if res then run else liftIO $ sendStop sv
    yield a r = do
        res <- liftIO $ send sv (ChildYield a)
        liftIO (enqueueFIFO sv q r)
        if res then run else liftIO $ sendStop sv

-------------------------------------------------------------------------------
-- Parallel
-------------------------------------------------------------------------------

{-# NOINLINE runOne #-}
runOne :: MonadIO m => SVar m a -> Stream m a -> m ()
runOne sv m = (runStream m) (Just sv) stop single yield

    where

    stop = liftIO $ sendStop sv
    sendit a = liftIO $ send sv (ChildYield a)
    single a = sendit a >> stop
    -- XXX there is no flow control in parallel case. We should perhaps use a
    -- queue and queue it back on that and exit the thread when the outputQueue
    -- overflows. Parallel is dangerous because it can accumulate unbounded
    -- output in the buffer.
    yield a r = void (sendit a) >> runOne sv r

-------------------------------------------------------------------------------
-- Ahead
-------------------------------------------------------------------------------

-- Lookahead streams can execute multiple tasks concurrently, ahead of time,
-- but always serve them in the same order as they appear in the stream. To
-- implement lookahead streams efficiently we assign a sequence number to each
-- task when the task is picked up for execution. When the task finishes, the
-- output is tagged with the same sequence number and we rearrange the outputs
-- in sequence based on that number.
--
-- To explain the mechanism imagine that the current task at the head of the
-- stream has a "token" to yield to the outputQueue. The ownership of the token
-- is determined by the current sequence number is maintained in outputHeap.
-- Sequence number is assigned when a task is queued. When a thread dequeues a
-- task it picks up the sequence number as well and when the output is ready it
-- uses the sequence number to queue the output to the outputQueue.
--
-- The thread with current sequence number sends the output directly to the
-- outputQueue. Other threads push the output to the outputHeap. When the task
-- being queued on the heap is a stream of many elements we evaluate only the
-- first element and keep the rest of the unevaluated computation in the heap.
-- When such a task gets the "token" for outputQueue it evaluates and directly
-- yields all the elements to the outputQueue without checking for the
-- "token".
--
-- Note that no two outputs in the heap can have the same sequence numbers and
-- therefore we do not need a stable heap. We have also separated the buffer
-- for the current task (outputQueue) and the pending tasks (outputHeap) so
-- that the pending tasks cannot interfere with the current task. Note that for
-- a single task just the outputQueue is enough and for the case of many
-- threads just a heap is good enough. However we balance between these two
-- cases, so that both are efficient.
--
-- For bigger streams it may make sense to have separate buffers for each
-- stream. However, for singleton streams this may become inefficient. However,
-- if we do not have separate buffers, then the streams that come later in
-- sequence may hog the buffer, hindering the streams that are ahead. For this
-- reason we have a single element buffer limitation for the streams being
-- executed in advance.
--
-- This scheme works pretty efficiently with less than 40% extra overhead
-- compared to the Async streams where we do not have any kind of sequencing of
-- the outputs. It is especially devised so that we are most efficient when we
-- have short tasks and need just a single thread. Also when a thread yields
-- many items it can hold lockfree access to the outputQueue and do it
-- efficiently.
--
-- XXX Maybe we can start the ahead threads at a lower cpu and IO priority so
-- that they do not hog the resources and hinder the progress of the threads in
-- front of them.

-- Left associated ahead expressions are expensive. We start a new SVar for
-- each left associative expression. The queue is used only for right
-- associated expression, we queue the right expression and execute the left.
-- Thererefore the queue never has more than on item in it.
{-# INLINE enqueueAhead #-}
enqueueAhead :: SVar m a -> IORef ([Stream m a], Int) -> Stream m a -> IO ()
enqueueAhead sv q m = do
    w <- readIORef $ waitingForWork sv
    atomicModifyIORefCAS_ q $ \ case
        ([], n) -> ([m], n + 1)  -- increment sequence
        _ -> error "not empty"
    when w $ do
        writeBarrier
        void $ tryPutMVar (doorBell sv) ()
        atomicModifyIORefCAS_ (waitingForWork sv) (const False)

-- Normally the thread that has the token should never go away. The token gets
-- handed over to another thread, but someone or the other has the token at any
-- point of time. But if the task that has the token finds that the outputQueue
-- is full, in that case it can go away without even handing over the token to
-- another thread. In that case it sets the nextSequence number in the heap its
-- own sequence number before going away. To handle this case, any task that
-- does not have the token tries to dequeue from the heap first before
-- dequeuing from the work queue. If it finds that the task at the top of the
-- heap is the one that owns the current sequence number then it grabs the
-- token and starts with that.
runqueueAhead :: MonadIO m => SVar m a -> IORef ([Stream m a], Int) -> m ()
runqueueAhead sv q = runHeap

    where

    maxHeap = 1500

    toHeap seqNo ent = do
        hp <- liftIO $ atomicModifyIORefCAS (outputHeap sv) $ \(h, snum) ->
            ((H.insert (Entry seqNo ent) h, snum), h)
        if H.size hp <= maxHeap
        then runHeap
        else liftIO $ sendStop sv

    singleToHeap seqNo a = toHeap seqNo (AheadEntryPure a)
    yieldToHeap seqNo a r = toHeap seqNo (AheadEntryStream (a `cons` r))

    singleOutput seqNo a = do
        continue <- liftIO $ send sv (ChildYield a)
        if continue
        then runQueueToken seqNo
        else liftIO $ do
            atomicModifyIORefCAS_ (outputHeap sv) $ \(h, _) -> (h, seqNo + 1)
            sendStop sv

    yieldOutput seqNo a r = do
        continue <- liftIO $ send sv (ChildYield a)
        if continue
        then (runStream r) (Just sv) (runQueueToken seqNo)
                                     (singleOutput seqNo)
                                     (yieldOutput seqNo)
        else liftIO $ do
            atomicModifyIORefCAS_ (outputHeap sv) $ \(h, _) ->
                (H.insert (Entry seqNo (AheadEntryStream r)) h, seqNo)
            sendStop sv

    {-# INLINE runQueueToken #-}
    runQueueToken prevSeqNo = do
        work <- dequeue
        case work of
            Nothing -> do
                liftIO $ atomicModifyIORefCAS_ (outputHeap sv) $ \(h, _) ->
                    (h, prevSeqNo + 1)
                runHeap
            Just (m, seqNo) -> do
                if seqNo == prevSeqNo + 1
                then
                    (runStream m) (Just sv) (runQueueToken seqNo)
                                            (singleOutput seqNo)
                                            (yieldOutput seqNo)
                else do
                    liftIO $ atomicModifyIORefCAS_ (outputHeap sv) $ \(h, _) ->
                        (h, prevSeqNo + 1)
                    (runStream m) (Just sv) runHeap
                                            (singleToHeap seqNo)
                                            (yieldToHeap seqNo)
    runQueueNoToken = do
        work <- dequeue
        case work of
            Nothing -> runHeap
            Just (m, seqNo) -> do
                if seqNo == 0
                then
                    (runStream m) (Just sv) (runQueueToken seqNo)
                                            (singleOutput seqNo)
                                            (yieldOutput seqNo)
                else
                    (runStream m) (Just sv) runHeap
                                            (singleToHeap seqNo)
                                            (yieldToHeap seqNo)

    {-# NOINLINE runHeap #-}
    runHeap = do
#ifdef DIAGNOSTICS
        liftIO $ do
            maxHp <- readIORef (maxHeapSize sv)
            (hp, _) <- readIORef (outputHeap sv)
            when (H.size hp > maxHp) $ writeIORef (maxHeapSize sv) (H.size hp)
#endif
        ent <- liftIO $ dequeueFromHeap (outputHeap sv)
        case ent of
            Nothing -> do
                done <- queueEmpty sv
                if done
                then liftIO $ sendStop sv
                else runQueueNoToken
            Just (Entry seqNo hent) -> do
                case hent of
                    AheadEntryPure a -> singleOutput seqNo a
                    AheadEntryStream r ->
                        (runStream r) (Just sv) (runQueueToken seqNo)
                                                (singleOutput seqNo)
                                                (yieldOutput seqNo)

    dequeue = liftIO $ do
        atomicModifyIORefCAS q $ \case
                ([], n) -> (([], n), Nothing)
                (x : [], n) -> (([], n), Just (x, n))
                _ -> error "more than one item on queue"

    dequeueFromHeap
        :: IORef (Heap (Entry Int (AheadHeapEntry m a)), Int)
        -> IO (Maybe (Entry Int (AheadHeapEntry m a)))
    dequeueFromHeap hpRef = do
        atomicModifyIORefCAS hpRef $ \hp@(h, snum) -> do
            let r = H.uncons h
            case r of
                Nothing -> (hp, Nothing)
                Just (ent@(Entry seqNo _ev), hp') ->
                    if (seqNo == snum)
                    then ((hp', seqNo), Just ent)
                    else (hp, Nothing)

-- Thread tracking is needed for two reasons:
--
-- 1) Killing threads on exceptions. Threads may not be left to go away by
-- themselves because they may run for significant times before going away or
-- worse they may be stuck in IO and never go away.
--
-- 2) To know when all threads are done and the stream has ended.

{-# NOINLINE addThread #-}
addThread :: MonadIO m => SVar m a -> ThreadId -> m ()
addThread sv tid =
    liftIO $ modifyIORef (runningThreads sv) (S.insert tid)

{-
{-# INLINE delThread #-}
delThread :: MonadIO m => SVar m a -> ThreadId -> m ()
delThread sv tid =
    liftIO $ modifyIORef (runningThreads sv) $ (\s -> S.delete tid s)
-}

-- If present then delete else add. This takes care of out of order add and
-- delete i.e. a delete arriving before we even added a thread.
-- This occurs when the forked thread is done even before the 'addThread' right
-- after the fork gets a chance to run.
{-# INLINE modifyThread #-}
modifyThread :: MonadIO m => SVar m a -> ThreadId -> m ()
modifyThread sv tid = do
    changed <- liftIO $ atomicModifyIORefCAS (runningThreads sv) $ \old ->
        if (S.member tid old)
        then let new = (S.delete tid old) in (new, new)
        else let new = (S.insert tid old) in (new, old)
    if null changed
    then liftIO $ do
        writeBarrier
        void $ tryPutMVar (doorBell sv) ()
    else return ()

-- | This is safe even if we are adding more threads concurrently because if
-- a child thread is adding another thread then anyway 'runningThreads' will
-- not be empty.
{-# INLINE allThreadsDone #-}
allThreadsDone :: MonadIO m => SVar m a -> m Bool
allThreadsDone sv = liftIO $ S.null <$> readIORef (runningThreads sv)

{-# NOINLINE handleChildException #-}
handleChildException :: SVar m a -> SomeException -> IO ()
handleChildException sv e = do
    tid <- myThreadId
    void $ send sv (ChildStop tid (Just e))

#ifdef DIAGNOSTICS
recordMaxWorkers :: MonadIO m => SVar m a -> m ()
recordMaxWorkers sv = liftIO $ do
    active <- readIORef (activeWorkers sv)
    maxWrk <- readIORef (maxWorkers sv)
    when (active > maxWrk) $ writeIORef (maxWorkers sv) active
    modifyIORef (totalDispatches sv) (+1)
#endif

{-# NOINLINE pushWorker #-}
pushWorker :: MonadAsync m => SVar m a -> m ()
pushWorker sv = do
    liftIO $ atomicModifyIORefCAS_ (activeWorkers sv) $ \n -> n + 1
#ifdef DIAGNOSTICS
    recordMaxWorkers sv
#endif
    doFork (runqueue sv) (handleChildException sv) >>= addThread sv

-- | In contrast to pushWorker which always happens only from the consumer
-- thread, a pushWorkerPar can happen concurrently from multiple threads on the
-- producer side. So we need to use a thread safe modification of
-- runningThreads. Alternatively, we can use a CreateThread event to avoid
-- using a CAS based modification.
{-# NOINLINE pushWorkerPar #-}
pushWorkerPar :: MonadAsync m => SVar m a -> Stream m a -> m ()
pushWorkerPar sv m = do
    -- We do not use activeWorkers in case of ParallelVar but still there is no
    -- harm in maintaining it correctly.
#ifdef DIAGNOSTICS
    liftIO $ atomicModifyIORefCAS_ (activeWorkers sv) $ \n -> n + 1
    recordMaxWorkers sv
#endif
    doFork (runOne sv m) (handleChildException sv) >>= modifyThread sv

-- XXX When the queue is LIFO we can put a limit on the number of dispatches.
-- Also, if a worker blocks on the output queue we can decide if we want to
-- block or make it go away entirely, depending on the number of workers and
-- the type of the queue.
{-# INLINE sendWorkerWait #-}
sendWorkerWait :: MonadAsync m => SVar m a -> m ()
sendWorkerWait sv = do
    -- When there is no output seen we dispatch more workers to help out if
    -- there is work pending in the work queue. But we wait a little while
    -- and check the output again so that we are not too aggressive.
    -- If there is no output pending to process and there is no worker to be
    -- sent then we block, so that we do not keep looping fruitlessly.

    -- Note that we are guaranteed to have at least one outstanding worker when
    -- we enter this function. So if we sleep we are guaranteed to be woken up.
    -- Note that the worker count is only decremented during event processing
    -- in fromStreamVar and therefore it is safe to read and use it without a
    -- lock.

    -- XXX we should wait in such a way so that we can be immediately
    -- interrupted by a doorBell. We can sleep on the doorbell and use a
    -- monitor thread to send us a doorbell.
    liftIO $ threadDelay 200
    (_, n) <- liftIO $ readIORef (outputQueue sv)
    when (n <= 0) $ do
        -- The queue may be empty temporarily if the worker has dequeued the
        -- work item but has not enqueued the remaining part yet. For the same
        -- reason, a worker may come back if it tries to dequeue and finds the
        -- queue empty, even though the whole work has not finished yet.
        done <- queueEmpty sv
        if not done
        then do
            cnt <- liftIO $ readIORef $ activeWorkers sv
            -- Note that we may deadlock if the previous workers (tasks in the
            -- stream) wait/depend on the future workers (tasks in the stream)
            -- executing. In that case we should either configure the maxWorker
            -- count to higher or use parallel style instead of ahead or async
            -- style.
            if (cnt < 1500)
            then do
                pushWorker sv
                sendWorkerWait sv
            else liftIO $ withDBGMVar sv "sendWorkerWait: wait for workers"
                                      $ takeMVar (doorBell sv)
        else do
            -- XXX Assert here that if the heap is not empty then there is at
            -- least one outstanding worker. Otherwise we could be sleeping
            -- forever.
            --
            -- We found that the queue was empty, but it may be empty
            -- temporarily, when we checked it. If that's the case we might
            -- sleep indefinitely unless the active workers produce some
            -- output. We may deadlock specially if the otuput from the other
            -- workers depends on the future workers that we may never send.
            -- So in case the queue was temporarily empty set a flag to inform
            -- the enqueue to send us a doorbell.

            liftIO $ atomicModifyIORefCAS_ (waitingForWork sv) $ const True
            liftIO $ writeBarrier

            -- check again, this time we have set the waitingForWork flag so we
            -- are guaranteed to get a doorbell in case the status changed
            -- before we could sleep.
            --
            -- Note that this is just a best effort mechanism to avoid a
            -- deadlock. Deadlocks may still happen if for some weird reason
            -- the consuming computation shares an MVar or some other resource
            -- with the producing computation and gets blocked on that resource
            -- and therefore cannot do any pushworker to add more threads to
            -- the producer. In such cases the programmer should use a parallel
            -- style so that all the producers are scheduled immediately and
            -- unconditionally. We can also use a separate monitor thread to
            -- push workers instead of pushing them from the consumer, but then
            -- we are no longer using pull based concurrency rate adaptation.
            --
            -- XXX update this in the tutorial.
            emp <- queueEmpty sv
            when emp $
                liftIO $ withDBGMVar sv "sendWorkerWait: nothing to do"
                                     $ takeMVar (doorBell sv)

-- | Pull a stream from an SVar.
{-# NOINLINE fromStreamVar #-}
fromStreamVar :: MonadAsync m => SVar m a -> Stream m a
fromStreamVar sv = Stream $ \_ stp sng yld -> do
    if svarStyle sv == ParallelVar
    then liftIO $ withDBGMVar sv "fromStreamVar: doorbell"
                              $ takeMVar (doorBell sv)
    else do
        res <- liftIO $ tryTakeMVar (doorBell sv)
        when (isNothing res) $ sendWorkerWait sv

#ifdef DIAGNOSTICS
    (list, len) <- liftIO $ atomicModifyIORefCAS (outputQueue sv) $ \x -> (([],0), x)
    oqLen <- liftIO $ readIORef (maxOutQSize sv)
    when (len > oqLen) $ liftIO $ writeIORef (maxOutQSize sv) len
#else
    (list, _) <- liftIO $ atomicModifyIORefCAS (outputQueue sv) $ \x -> (([],0), x)
#endif
    -- Reversing the output is important to guarantee that we process the
    -- outputs in the same order as they were generated by the constituent
    -- streams.
    (runStream $ processEvents (reverse list)) Nothing stp sng yld

    where

    handleException e tid = do
        liftIO $ atomicModifyIORefCAS_ (activeWorkers sv) $ \n -> n - 1
        modifyThread sv tid
        -- XXX implement kill async exception handling
        -- liftIO $ readIORef (runningThreads sv) >>= mapM_ killThread
        throwM e

    allDone stp = do
#ifdef DIAGNOSTICS
#ifdef DIAGNOSTICS_VERBOSE
            svInfo <- liftIO $ dumpSVar sv
            liftIO $ hPutStrLn stderr $ "fromStreamVar done\n" ++ svInfo
#endif
#endif
            stp

    {-# INLINE processEvents #-}
    processEvents [] = Stream $ \_ stp sng yld -> do
        workersDone <- allThreadsDone sv
        if not workersDone
        then (runStream (fromStreamVar sv)) Nothing stp sng yld
        else
            if svarStyle sv == ParallelVar
            then allDone stp
            else do
                -- All the workers may come back if the outputQueue is full.
                -- So check if there is work pending and kick off a worker to
                -- finish it.
                -- XXX we should perhaps kickoff even before we start
                -- processing if the active workers are zero, rather than at
                -- the end of processing the outputQueue. But some thread stop
                -- events may be in the outputQueue itself, so we may not be
                -- able to determine the correct number of workers before we
                -- process it.
                -- XXX It is necessary to do this before the processing to keep
                -- minimal concurrency always.
                -- We should have a separate queue for Stop events so that we
                -- can process them before the outputs and send workers
                -- promptly when needed.
                heapDone <-
                    if (svarStyle sv == AheadVar)
                    then do
                        (hp, _) <- liftIO $ readIORef (outputHeap sv)
                        return (H.size hp <= 0)
                    else return True
                queueDone <- queueEmpty sv
                if queueDone && heapDone
                then allDone stp
                else do
                    pushWorker sv
                    (runStream (fromStreamVar sv)) Nothing stp sng yld

    processEvents (ev : es) = Stream $ \_ stp sng yld -> do
        let continue = (runStream (processEvents es)) Nothing stp sng yld
            yield a  = yld a (processEvents es)

        case ev of
            ChildYield a -> yield a
            ChildStop tid e ->
                case e of
                    Nothing -> do
                        let active = activeWorkers sv
                        liftIO $ atomicModifyIORefCAS_ active $ \n -> n - 1
                        modifyThread sv tid >> continue
                    Just ex -> handleException ex tid

getFifoSVar :: MonadIO m => SVarStyle -> IO (SVar m a)
getFifoSVar ctype = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q       <- newQ
#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let sv =
            SVar { outputQueue    = outQ
                 , doorBell       = outQMv
                 , outputHeap     = undefined
                 , runningThreads = running
                 , workQueue      = undefined
                 , runqueue       = runqueueFIFO sv q
                 , enqueue        = enqueueFIFO sv q
                 , queueEmpty     = liftIO $ nullQ q
                 , waitingForWork = wfw
                 , svarStyle      = ctype
                 , activeWorkers  = active
#ifdef DIAGNOSTICS
                 , totalDispatches = disp
                 , maxWorkers   = maxWrk
                 , maxOutQSize  = maxOq
                 , maxHeapSize  = maxHs
                 , maxWorkQSize = maxWq
#endif
                 }
     in return sv

getLifoSVar :: MonadIO m => SVarStyle -> IO (SVar m a)
getLifoSVar ctype = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q <- newIORef []
#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let checkEmpty = null <$> liftIO (readIORef q)
    let sv =
            SVar { outputQueue    = outQ
                 , doorBell       = outQMv
                 , outputHeap     = undefined
                 , runningThreads = running
                 , workQueue      = undefined
                 , runqueue       = runqueueLIFO sv q
                 , enqueue        = enqueueLIFO sv q
                 , queueEmpty     = checkEmpty
                 , waitingForWork = wfw
                 , svarStyle      = ctype
                 , activeWorkers  = active
#ifdef DIAGNOSTICS
                 , maxWorkers   = maxWrk
                 , totalDispatches = disp
                 , maxOutQSize  = maxOq
                 , maxHeapSize  = maxHs
                 , maxWorkQSize = maxWq
#endif
                 }
     in return sv

getParSVar :: SVarStyle -> IO (SVar m a)
getParSVar style = do
    outQ    <- newIORef ([], 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif
    let sv =
            SVar { outputQueue    = outQ
                 , doorBell       = outQMv
                 , outputHeap     = undefined
                 , runningThreads = running
                 , workQueue      = undefined
                 , runqueue       = undefined
                 , enqueue        = undefined
                 , queueEmpty     = undefined
                 , waitingForWork = wfw
                 , svarStyle      = style
                 , activeWorkers  = active
#ifdef DIAGNOSTICS
                 , totalDispatches = disp
                 , maxWorkers   = maxWrk
                 , maxOutQSize  = maxOq
                 , maxHeapSize  = maxHs
                 , maxWorkQSize = maxWq
#endif
                 }
     in return sv

getAheadSVar :: MonadIO m => SVarStyle -> IO (SVar m a)
getAheadSVar style = do
    outQ    <- newIORef ([], 0)
    outH    <- newIORef (H.empty, 0)
    outQMv  <- newEmptyMVar
    active  <- newIORef 0
    wfw     <- newIORef False
    running <- newIORef S.empty
    q <- newIORef ([], -1)

#ifdef DIAGNOSTICS
    disp <- newIORef 0
    maxWrk <- newIORef 0
    maxOq  <- newIORef 0
    maxHs  <- newIORef 0
    maxWq  <- newIORef 0
#endif

    let checkEmpty = liftIO $ do
                        (xs, _) <- readIORef q
                        return $ null xs
    let sv =
            SVar { outputQueue    = outQ
                 , doorBell       = outQMv
                 , outputHeap     = outH
                 , runningThreads = running
                 , workQueue      = q
                 , runqueue       = runqueueAhead sv q
                 , enqueue        = undefined
                 , queueEmpty     = checkEmpty
                 , waitingForWork = wfw
                 , svarStyle      = style
                 , activeWorkers  = active

#ifdef DIAGNOSTICS
                 , totalDispatches = disp
                 , maxWorkers   = maxWrk
                 , maxOutQSize  = maxOq
                 , maxHeapSize  = maxHs
                 , maxWorkQSize = maxWq
#endif
                 }
     in return sv

-- | Create a new empty SVar.
newEmptySVar :: MonadAsync m => SVarStyle -> m (SVar m a)
newEmptySVar style = do
    liftIO $
        case style of
            WAsyncVar -> getFifoSVar style
            AsyncVar -> getLifoSVar style
            ParallelVar -> getParSVar style
            AheadVar -> getAheadSVar style

-- | Create a new SVar and enqueue one stream computation on it.
{-# INLINABLE newStreamVar1 #-}
newStreamVar1 :: MonadAsync m => SVarStyle -> Stream m a -> m (SVar m a)
newStreamVar1 style m = do
    sv <- newEmptySVar style
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    if style == ParallelVar
    then pushWorkerPar sv m
    else do
        liftIO $ (enqueue sv) m
        pushWorker sv
    return sv

-- | Create a new SVar and enqueue one stream computation on it.
{-# INLINABLE newStreamVarAhead #-}
newStreamVarAhead :: MonadAsync m => Stream m a -> m (SVar m a)
newStreamVarAhead m = do
    sv <- newEmptySVar AheadVar
    -- Note: We must have all the work on the queue before sending the
    -- pushworker, otherwise the pushworker may exit before we even get a
    -- chance to push.
    liftIO $ enqueueAhead sv (workQueue sv) m
    pushWorker sv
    return sv

-- | Write a stream to an 'SVar' in a non-blocking manner. The stream can then
-- be read back from the SVar using 'fromSVar'.
toStreamVar :: MonadAsync m => SVar m a -> Stream m a -> m ()
toStreamVar sv m = do
    liftIO $ (enqueue sv) m
    done <- allThreadsDone sv
    -- XXX This is safe only when called from the consumer thread or when no
    -- consumer is present.  There may be a race if we are not running in the
    -- consumer thread.
    when done $ pushWorker sv

------------------------------------------------------------------------------
-- Running streams concurrently
------------------------------------------------------------------------------

-- Concurrency rate control.
--
-- Our objective is to create more threads on demand if the consumer is running
-- faster than us. As soon as we encounter a concurrent composition we create a
-- push pull pair of threads. We use an SVar for communication between the
-- consumer, pulling from the SVar and the producer who is pushing to the SVar.
-- The producer creates more threads if the SVar drains and becomes empty, that
-- is the consumer is running faster.
--
-- XXX Note 1: This mechanism can be problematic if the initial production
-- latency is high, we may end up creating too many threads. So we need some
-- way to monitor and use the latency as well. Having a limit on the dispatches
-- (programmer controlled) may also help.
--
-- TBD Note 2: We may want to run computations at the lower level of the
-- composition tree serially even when they are composed using a parallel
-- combinator. We can use 'serial' in place of 'async' and 'wSerial' in
-- place of 'wAsync'. If we find that an SVar immediately above a computation
-- gets drained empty we can switch to parallelizing the computation.  For that
-- we can use a state flag to fork the rest of the computation at any point of
-- time inside the Monad bind operation if the consumer is running at a faster
-- speed.
--
-- TBD Note 3: the binary operation ('parallel') composition allows us to
-- dispatch a chunkSize of only 1.  If we have to dispatch in arbitrary
-- chunksizes we will need to compose the parallel actions using a data
-- constructor (A Free container) instead so that we can divide it in chunks of
-- arbitrary size before dispatching. If the stream is composed of
-- hierarchically composed grains of different sizes then we can always switch
-- to a desired granularity depending on the consumer speed.
--
-- TBD Note 4: for pure work (when we are not in the IO monad) we can divide it
-- into just the number of CPUs.

-- | Join two computations on the currently running 'SVar' queue for concurrent
-- execution.  When we are using parallel composition, an SVar is passed around
-- as a state variable. We try to schedule a new parallel computation on the
-- SVar passed to us. The first time, when no SVar exists, a new SVar is
-- created.  Subsequently, 'joinStreamVarAsync' may get called when a computation
-- already scheduled on the SVar is further evaluated. For example, when (a
-- `parallel` b) is evaluated it calls a 'joinStreamVarAsync' to put 'a' and 'b' on
-- the current scheduler queue.
--
-- The 'SVarStyle' required by the current composition context is passed as one
-- of the parameters.  If the scheduling and composition style of the new
-- computation being scheduled is different than the style of the current SVar,
-- then we create a new SVar and schedule it on that.  The newly created SVar
-- joins as one of the computations on the current SVar queue.
--
-- Cases when we need to switch to a new SVar:
--
-- * (x `parallel` y) `parallel` (t `parallel` u) -- all of them get scheduled on the same SVar
-- * (x `parallel` y) `parallel` (t `async` u) -- @t@ and @u@ get scheduled on a new child SVar
--   because of the scheduling policy change.
-- * if we 'adapt' a stream of type 'async' to a stream of type
--   'Parallel', we create a new SVar at the transitioning bind.
-- * When the stream is switching from disjunctive composition to conjunctive
--   composition and vice-versa we create a new SVar to isolate the scheduling
--   of the two.

forkSVarAsync :: MonadAsync m => SVarStyle -> Stream m a -> Stream m a -> Stream m a
forkSVarAsync style m1 m2 = Stream $ \_ stp sng yld -> do
    sv <- newStreamVar1 style (concurrently m1 m2)
    (runStream (fromStreamVar sv)) Nothing stp sng yld
    where
    concurrently ma mb = Stream $ \svr stp sng yld -> do
        liftIO $ enqueue (fromJust svr) mb
        (runStream ma) svr stp sng yld

{-# INLINE joinStreamVarAsync #-}
joinStreamVarAsync :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
joinStreamVarAsync style m1 m2 = Stream $ \svr stp sng yld ->
    case svr of
        Just sv | svarStyle sv == style ->
            liftIO ((enqueue sv) m2) >> (runStream m1) svr stp sng yld
        _ -> runStream (forkSVarAsync style m1 m2) Nothing stp sng yld

{-# NOINLINE forkSVarPar #-}
forkSVarPar :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
forkSVarPar m r = Stream $ \_ stp sng yld -> do
    sv <- newEmptySVar ParallelVar
    pushWorkerPar sv m
    pushWorkerPar sv r
    (runStream (fromStreamVar sv)) Nothing stp sng yld

{-# INLINE joinStreamVarPar #-}
joinStreamVarPar :: MonadAsync m
    => SVarStyle -> Stream m a -> Stream m a -> Stream m a
joinStreamVarPar style m1 m2 = Stream $ \svr stp sng yld ->
    case svr of
        Just sv | svarStyle sv == style -> do
            pushWorkerPar sv m1 >> (runStream m2) svr stp sng yld
        _ -> runStream (forkSVarPar m1 m2) Nothing stp sng yld

forkSVarAhead :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
forkSVarAhead m1 m2 = Stream $ \_ stp sng yld -> do
        sv <- newStreamVarAhead (concurrently m1 m2)
        (runStream (fromStreamVar sv)) Nothing stp sng yld
    where
    concurrently ma mb = Stream $ \svr stp sng yld -> do
        liftIO $ enqueueAhead (fromJust svr) (workQueue (fromJust svr)) mb
        (runStream ma) Nothing stp sng yld

{-# INLINE ahead #-}
ahead :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
ahead m1 m2 = Stream $ \svr stp sng yld -> do
    case svr of
        Just sv | svarStyle sv == AheadVar -> do
            liftIO $ enqueueAhead sv (workQueue sv) m2
            -- Always run the left side on a new SVar to avoid complexity in
            -- sequencing results. This means the left side cannot further
            -- split into more ahead computations on the same SVar.
            (runStream m1) Nothing stp sng yld
        _ -> runStream (forkSVarAhead m1 m2) Nothing stp sng yld

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using ahead.
{-# INLINE consMAhead #-}
consMAhead :: MonadAsync m => m a -> Stream m a -> Stream m a
consMAhead m r = once m `ahead` r

------------------------------------------------------------------------------
-- Semigroup and Monoid style compositions for parallel actions
------------------------------------------------------------------------------

{-# INLINE async #-}
async :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
async = joinStreamVarAsync AsyncVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using async.
{-# INLINE consMAsync #-}
consMAsync :: MonadAsync m => m a -> Stream m a -> Stream m a
consMAsync m r = once m `async` r

{-# INLINE wAsync #-}
wAsync :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
wAsync = joinStreamVarAsync WAsyncVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using wAsync.
{-# INLINE consMWAsync #-}
consMWAsync :: MonadAsync m => m a -> Stream m a -> Stream m a
consMWAsync m r = once m `wAsync` r

{-# INLINE parallel #-}
parallel :: MonadAsync m => Stream m a -> Stream m a -> Stream m a
parallel = joinStreamVarPar ParallelVar

-- | XXX we can implement it more efficienty by directly implementing instead
-- of combining streams using parallel.
{-# INLINE consMParallel #-}
consMParallel :: MonadAsync m => m a -> Stream m a -> Stream m a
consMParallel m r = once m `parallel` r

-------------------------------------------------------------------------------
-- Functor instace is the same for all types
-------------------------------------------------------------------------------

instance Monad m => Functor (Stream m) where
    fmap f m = Stream $ \_ stp sng yld ->
        let single    = sng . f
            yield a r = yld (f a) (fmap f r)
        in (runStream m) Nothing stp single yield

------------------------------------------------------------------------------
-- Alternative & MonadPlus
------------------------------------------------------------------------------

_alt :: Stream m a -> Stream m a -> Stream m a
_alt m1 m2 = Stream $ \_ stp sng yld ->
    let stop  = runStream m2 Nothing stp sng yld
    in runStream m1 Nothing stop sng yld

------------------------------------------------------------------------------
-- Stream to stream function application
------------------------------------------------------------------------------

applyWith :: MonadAsync m
    => SVarStyle -> (Stream m a -> Stream m b) -> Stream m a -> Stream m b
applyWith style f m = Stream $ \svr stp sng yld -> do
    sv <- newStreamVar1 style m
    runStream (f $ fromStreamVar sv) svr stp sng yld

------------------------------------------------------------------------------
-- Stream runner function application
------------------------------------------------------------------------------

runWith :: MonadAsync m
    => SVarStyle -> (Stream m a -> m b) -> Stream m a -> m b
runWith style f m = do
    sv <- newStreamVar1 style m
    f $ fromStreamVar sv

------------------------------------------------------------------------------
-- Zipping
------------------------------------------------------------------------------

zipWith :: (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipWith f m1 m2 = go m1 m2
    where
    go mx my = Stream $ \_ stp sng yld -> do
        let merge a ra =
                let single2 b = sng (f a b)
                    yield2 b rb = yld (f a b) (go ra rb)
                 in (runStream my) Nothing stp single2 yield2
        let single1 a   = merge a nil
            yield1 a ra = merge a ra
        (runStream mx) Nothing stp single1 yield1

zipAsyncWith :: MonadAsync m
    => (a -> b -> c) -> Stream m a -> Stream m b -> Stream m c
zipAsyncWith f m1 m2 = Stream $ \_ stp sng yld -> do
    ma <- mkAsync m1
    mb <- mkAsync m2
    (runStream (zipWith f ma mb)) Nothing stp sng yld

    where

    mkAsync :: MonadAsync m => Stream m a -> m (Stream m a)
    mkAsync m = newStreamVar1 AsyncVar m
        >>= return . fromStreamVar

-------------------------------------------------------------------------------
-- Transformers
-------------------------------------------------------------------------------

instance MonadTrans Stream where
    lift = once
