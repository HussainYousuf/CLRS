import Control.Concurrent (ThreadId, forkIO, myThreadId, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Monad (forever)
import Control.Monad.ST (ST, runST)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromJust)
import Data.STRef (newSTRef)
import System.IO (BufferMode (LineBuffering, NoBuffering), hGetContents, hPutStrLn, hSetBinaryMode, hSetBuffering, stdin, stdout)
import System.Process.Internals (ProcRetHandles (hStdInput))
import Text.Printf (printf)

-- main :: IO ()
-- main = forever $ do
--     xs <- getLine
--     forkIO $ reminder $ read xs
--     printf "OK will remind you in %s seconds\n" xs
--   where
--     reminder :: Int -> IO ()
--     reminder seconds = forever $ do
--         threadDelay $ seconds * 10^6
--         printf "alarm at %d seconds triggered\n" seconds

f :: IO ThreadId
f = myThreadId

p :: IO ThreadId -> IO ()
p = (>>= print)

foo = do
    a <- newEmptyMVar
    myThreadId >>= print
    print "main done"
    forkIO $ p f
    takeMVar a


-- type Stream a = MVar (Item a)
-- data Item a = Item a (Stream a)
-- -- Item a MVar (Item a MVar ())

-- data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))