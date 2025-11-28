--
-- INFOB3CC Concurrency
-- Practical 1: IBAN calculator
--
-- http://ics.uu.nl/docs/vakken/b3cc/assessment.html
--
module IBAN (

  Mode(..), Config(..),
  mtest,
  count, list, search

) where

import Control.Concurrent
import Crypto.Hash.SHA1
import Data.Atomics                                       ( readForCAS, casIORef, peekTicket )
import Data.IORef
import Data.List                                          ( elemIndex )
import Data.Word
import Data.Maybe                                         ( fromJust )
import System.Environment
import System.IO
import Data.ByteString.Char8                              ( ByteString )
import qualified Data.ByteString                          as B
import qualified Data.ByteString.Char8                    as B8


-- -----------------------------------------------------------------------------
-- 0. m-test
-- -----------------------------------------------------------------------------

-- Perform the m-test on 'number'. Use `div` and `mod` to extract digits from
-- the number; do not use `show`, as it is too slow.

-- notes 
-- pak cijfer van rechts naar links, least significant digit eerst 
-- gewicht start bij 1 (laatste cijfer) gaat door tot alle cijfers op zijn
-- pak sommatie van gewicht * cijfer, geldig als som `mod` m == 0 

mtest :: Int -> Int -> Bool
mtest m number = loop number 1 0
  where
    loop 0 _ acc = acc `mod` m == 0
    loop r w acc =
      let (q, d) = r `quotRem` 10     -- quotrem geeft quotient en remainder
          acc'   = acc + w * d
      in loop q (w + 1) acc'


-- -----------------------------------------------------------------------------
-- 1. Counting mode (3pt)
-- -----------------------------------------------------------------------------

count :: Config -> IO Int
count config = do
  let b = cfgLower config
      e = cfgUpper config
      m = cfgModulus config
      p = cfgThreads config

  totalRef <- newIORef 0

  -- Start p threads, lokale telling uit eigen subrange 
  forkThreads p $ \i -> do
    let (start, len) = rangeFor b e p i
        local = countRange m start len
    -- Forceer de berekening 
    evaluate local
    -- Atomisch een keer toevoegen 
    casAdd totalRef local

  readIORef totalRef

-- Helper; verdeel ondergrens b en bovengrens e over p threads
-- geef start, lengte voor thread i
rangeFor :: Int -> Int -> Int -> Int -> (Int, Int)
rangeFor b e p i =
  let n  = e - b
      q  = n `div` p
      r  = n `mod` p
      start = b + i*q + min i r
      len   = q + (if i < r then 1 else 0)
  in (start, len)

-- lokale teller voor range (start, start+len)
countRange :: Int -> Int -> Int -> Int
countRange m start len =
  let end = start + len
      go x acc
        | x >= end  = acc
        | mtest m x = go (x+1) (acc+1)
        | otherwise = go (x+1) acc
  in go start 0

-- CAS update helper 
casAdd :: IORef Int -> Int -> IO ()
casAdd ref delta = do
  -- Forceer delta voor geen thunk in de kritieke sectie
  evaluate delta
  let loop = do
        ticket <- readForCAS ref
        let old = peekTicket ticket
            new = old + delta
        (ok, _) <- casIORef ref ticket new
        if ok then return () else loop
  loop

-- -----------------------------------------------------------------------------
-- 2. List mode (3pt)
-- -----------------------------------------------------------------------------

-- loop weer door bereik heen b tot e 
-- print sequenceNumber en accountNumber voor gehaalde m tests
-- meerdere threads tegelijk op verschillende delen van bereik 
-- sequence number oplopend op volgorde van verschijnen in output 
-- synchoniseren met MVars 

list :: Handle -> Config -> IO ()
list handle config = do
  let b = cfgLower   config
      e = cfgUpper   config
      m = cfgModulus config
      p = cfgThreads config

  seqVar <- newMVar 0   -- global sequence teller
  forkThreads p $ \i -> do
    let (start, len) = rangeFor b e p i
        end          = start + len
    -- Loop over alle mogelijke waarden in  subrange
    let loop x 
          | x >= end  = return ()
          | otherwise = do
              if mtest m x
                then do
                  -- krietieke sectie, dus MVar gebruiken 
                  -- pak teller, verhoog met 1, print regel, zet terug
                  n <- takeMVar seqVar
                  let n' = n + 1
                  hPutStrLn handle (show n' ++ " " ++ show x)
                  putMVar seqVar n'
                  loop (x+1)
                else loop (x+1)
    loop start

  return ()  -- niets terug geven output staat in handle


-- -----------------------------------------------------------------------------
-- 3. Search mode (4pt)
-- -----------------------------------------------------------------------------
-- zoek naar eerste bankrekeningnummer in bereik b tot e dat voldoet aan m-test
-- en waarvan SHA1 hash gelijk is aan gegeven query
-- p threads tegelijk op verschillende delen van bereik
-- zodra een thread een match vindt, moeten alle threads stoppen
-- resultaat is het gevonden bankrekeningnummer of Nothing als niets is gevonden. gebruik stop-flag en MVar voor resultaat 


type Job = (Int, Int)  -- (start,end)


-- maak vaste blokken van b e chunk grootte -> elke job is (start,end)
mkJobs :: Int -> Int -> Int -> [Job]
mkJobs b e chunk =
  let go s acc
        | s >= e    = reverse acc
        | otherwise =
            let end = min e (s + chunk)
            in go end ((s,end):acc)
  in go b []


--true als x m-test haalt en SHA1(show x) == query
candidateMatches :: Int -> ByteString -> Int -> Bool
candidateMatches m query x = mtest m x && checkHash query (show x)


-- zet stop flag en geef gevonden waarde door in MVar
publishFound :: IORef Bool -> MVar (Maybe Int) -> Int -> IO ()
publishFound stopFlag foundVar x = do
  writeIORef stopFlag True
  cur <- takeMVar foundVar
  case cur of
    Nothing -> putMVar foundVar (Just x)
    Just v  -> putMVar foundVar (Just v)


search :: Config -> ByteString -> IO (Maybe Int)
search config query = do
  let b = cfgLower   config
      e = cfgUpper   config
      m = cfgModulus config
      p = cfgThreads config

  let chunkSize = 2048
      jobs0     = mkJobs b e chunkSize

  queueVar <- newMVar jobs0 -- MVar [Job]
  foundVar <- newMVar Nothing -- MVar (Maybe Int)
  stopFlag <- newIORef False  -- IORef Bool


-- haal een job van de queue of Nothing als leeg
  let dequeueJob :: IO (Maybe Job)
      dequeueJob = do
        jobs <- takeMVar queueVar
        case jobs of
          []       -> putMVar queueVar [] >> return Nothing
          (j:rest) -> putMVar queueVar rest >> return (Just j)

      -- doorloop kandidaten van job, stop bij gevonden of einde
      processJob :: Job -> IO ()
      processJob (s,end) =
        let loop x
              | x >= end = return ()
              | otherwise = do
                  stopped <- readIORef stopFlag
                  if stopped
                    then return ()
                    else if candidateMatches m query x
                      then publishFound stopFlag foundVar x
                      else loop (x+1)
        in loop s
      -- pak jobs en verwerk-> stop bij stop-flag of lege queue
      worker :: Int -> IO ()
      worker _ = do
        stopped <- readIORef stopFlag
        if stopped
          then return ()
          else do
            mjob <- dequeueJob
            case mjob of
              Nothing  -> return ()
              Just job -> processJob job >> worker 0

  forkThreads p worker

  -- resultaat teruggeven, blokkeer tot gevonden of alle threads klaar 
  takeMVar foundVar


-- -----------------------------------------------------------------------------
-- Starting framework
-- -----------------------------------------------------------------------------

data Mode = Count | List | Search ByteString
  deriving Show

data Config = Config
  { cfgLower   :: !Int
  , cfgUpper   :: !Int
  , cfgModulus :: !Int
  , cfgThreads :: !Int
  }
  deriving Show

-- Evaluates a term, before continuing with the next IO operation.
--
evaluate :: a -> IO ()
evaluate x = x `seq` return ()

-- Forks 'n' threads. Waits until those threads have finished. Each thread
-- runs the supplied function given its thread ID in the range [0..n).
--
forkThreads :: Int -> (Int -> IO ()) -> IO ()
forkThreads n work = do
  -- Fork the threads and create a list of the MVars which
  -- per thread tell whether the work has finished.
  finishVars <- mapM work' [0 .. n - 1]
  -- Wait on all MVars
  mapM_ takeMVar finishVars
  where
    work' :: Int -> IO (MVar ())
    work' index = do
      var <- newEmptyMVar
      _   <- forkOn index (work index >> putMVar var ())
      return var

-- Checks whether 'value' has the expected hash.
--
checkHash :: ByteString -> String -> Bool
checkHash expected value = expected == hash (B8.pack value)
