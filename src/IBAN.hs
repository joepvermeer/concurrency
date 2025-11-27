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

  -- Start p threads 
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

list :: Handle -> Config -> IO ()
list handle config = do
  -- Implement list mode here!
  -- Remember to use "hPutStrLn handle" to write your output.
  undefined


-- -----------------------------------------------------------------------------
-- 3. Search mode (4pt)
-- -----------------------------------------------------------------------------

search :: Config -> ByteString -> IO (Maybe Int)
search config query = do
  -- Implement search mode here!
  undefined


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
