{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    Job (..),
    jobAdd,
    JobId, 
    JobStatus (..),
    JobDoneReason (..),
    jobStatus, 
    jobCancel,
    jobWait,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg = MsgJobAdd Job (ReplyChan JobId)
            | MsgJobStatus JobId (ReplyChan JobStatus)
            | MsgJobCancel JobId 
            | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
            | MsgJobDone JobId

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { 
    spcJobsPending :: [(JobId, Job)],
    spcJobCounter :: JobId,
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcWaiting :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcJobRunning :: Maybe (JobId, ThreadId),
    spcChan :: Chan SPCMsg
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  (<*>) = ap
  pure a = SPCM $ \state -> pure (a, state) 

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \s -> do
    (x, state') <- m s
    let SPCM f' = f x 
    f' state'

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }
-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  |
    JobUnknown
  deriving (Eq, Ord, Show)

schedule :: SPCM ()
schedule = do
  state <- get
  case (spcJobRunning state, spcJobsPending state) of
    (Nothing, (id, job) : jobs) -> do
      tid <- io $ forkIO $ do
        jobAction job
        send (spcChan state) $ MsgJobDone id
      put $ state 
        {
          spcJobRunning = Just (id, tid),
          spcJobsPending = jobs
        } 
    _ -> pure ()


jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone id reason = do
  state <- get
  case lookup id $ spcJobsPending state of
    Nothing -> pure ()
    Just _ -> do
      let waiters = filter (\(jobId, _) -> jobId == id) $ spcWaiting state
      let rest = filter (\(jobId, _) -> jobId /= id) $ spcWaiting state
      forM_ waiters $ \(_, rc) ->
        io $ reply rc $ Just reason
      put $ state 
        {
          spcWaiting = rest,
          spcJobsPending = removeAssoc id $ spcJobsPending state,
          spcJobsDone = (id, reason) : spcJobsDone state
        }

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC s) newjob = requestReply s $ MsgJobAdd newjob 

-- | Query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) id = requestReply c $ MsgJobStatus id

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) id = sendTo c $ MsgJobCancel id

jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io x = SPCM $ \state -> do
  unpacked <- x 
  pure (unpacked, state) 

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = do
  (a, _) <- f state
  pure a 

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rc -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $ state {
        spcJobsPending =
          (spcJobCounter state, job) : spcJobsPending state,
          spcJobCounter = JobId $ succ jobid
      }
      io $ reply rc $ JobId jobid
    MsgJobStatus id rc -> do 
      state <- get
      io $ reply rc $ case ( 
                              lookup id $ spcJobsPending state,
                              spcJobRunning state,
                              lookup id $ spcJobsDone state
                            ) of
        (Just _, _, _) -> JobPending
        (_, Just (running_job, _), _)
            | running_job == id -> JobRunning
        (_, _, Just reason) -> JobDone reason
        _ -> JobUnknown
    MsgJobCancel cancel_jobid -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, tid) | jobid == cancel_jobid -> do
          io $ killThread tid
          jobDone jobid DoneCancelled
        _ -> pure ()
    MsgJobWait id rc -> do
      state <- get
      case lookup id $ spcJobsDone state of
        Nothing -> put $ state {spcWaiting = (id, rc) : spcWaiting state}
        Just reason -> io $ reply rc $ Just reason 
    MsgJobDone id -> do
      state <- get
      case spcJobRunning state of
        Just (jobid, _)
          | jobid == id ->
              jobDone jobid Done
        _ -> pure ()



startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobRunning = Nothing,
            spcJobsDone = [],
            spcWaiting = [],
            spcChan = c
          }
  server <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  pure $ SPC server
