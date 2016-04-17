-----------------------------------------------------------------------------
--
-- Module      :  QSN.Analysis.SparQ
-- Copyright   :  (c) by Franz-Benjamin Mocnik, 2015
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QSN.Analysis.SparQ (
    SparQQuery(..),
    Calculus(..),
    ConstraintReasoningCommand(..),
    SparQResult(..),
    isUnmodifiedNetwork,
    isModifiedNetwork,
    isNotConsistent,
    isOtherResult,
    querySparQ,
    querySparQraw,
    querySparQ',
    querySparQraw') where

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.DeriveTH
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle
import Network.Simple.TCP
import qualified System.IO.Streams as Streams
import System.Process
import System.Random

data SparQQuery = LoadCalculus Calculus | ConstraintReasoning Calculus ConstraintReasoningCommand String deriving Eq
data Calculus = CalculusOPRA Int | Star deriving Eq
data ConstraintReasoningCommand = AClosure deriving Eq
data SparQResult = UnmodifiedNetwork | ModifiedNetwork String | NotConsistent String | OtherResult String deriving (Eq, Show)

$(derive makeIs ''SparQResult)

instance Show SparQQuery where
    show (LoadCalculus x) = "load-calculus " ++ show x
    show (ConstraintReasoning calc cmd x) = "constraint-reasoning " ++ show calc ++ " " ++ show cmd ++ " " ++ x

instance Show Calculus where
    show (CalculusOPRA n) = "opra-" ++ show n
    show Star = "*"

instance Show ConstraintReasoningCommand where
    show AClosure = "a-closure"

translateSparQ :: [String] -> [SparQResult]
translateSparQ = map makeResult where
    makeResult x
        | "Unmodified network.\n" `isPrefixOf` x = UnmodifiedNetwork
        | "Modified network.\n" `isPrefixOf` x = ModifiedNetwork . drop 14 $ x
        | "Not consistent.\n" `isPrefixOf` x = NotConsistent . drop 16 $ x
        | otherwise = OtherResult x

querySparQ :: [SparQQuery] -> IO [SparQResult]
querySparQ = fmap translateSparQ . querySparQraw . map show

querySparQ' :: [SparQQuery] -> IO [SparQResult]
querySparQ' = fmap translateSparQ . querySparQraw' . map show

-- --== CONNECTION BY PIPE

querySparQraw :: [String] -> IO [String]
querySparQraw cmds = do
    (Just hout, Just hin, _, _) <- createProcess (proc "sparq" ["-i"]) { std_in = CreatePipe, std_out = CreatePipe }
    _ <- readCompletePipe hin
    xs <- forM cmds $ \cmd -> do
        writeCompletePipe hout cmd
        readCompletePipe hin
    writeCompletePipe hout "quit"
    _ <- hGetContents hin
    return xs

readCompletePipe :: Handle -> IO String
readCompletePipe hin = T.unpack . stripSuffix' "\n" <$> readComplete' "" where
    readComplete' xs = do
        x <- TIO.hGetChunk hin
        let xs' = T.concat [xs, x]
        case T.stripSuffix "\nsparq> " xs' of
            Just x' -> return x'
            Nothing -> readComplete' xs'
    stripSuffix' s x = fromMaybe x . T.stripSuffix s $ x

writeCompletePipe :: Handle -> String -> IO ()
writeCompletePipe hout cmd = do
    hPutStr hout $ cmd ++ "\n"
    hFlush hout

-- --== CONNECTION BY TCP/IP

querySparQraw' :: [String] -> IO [String]
querySparQraw' cmds = do
    port <- show <$> (getStdRandom (randomR (49152, 65535)) :: IO Int)
    (_, Just handle, _, _) <- createProcess (proc "sparq" ["-i", "-p", port]) { std_out = CreatePipe }
    waitUntil handle "..." . connect "127.0.0.1" port $ \(connectionSocket, _) -> do
        s <- Streams.socketToStreams connectionSocket
        _ <- readComplete s
        xs <- forM cmds $ \cmd -> do
            writeComplete s cmd
            readComplete s
        writeComplete s "quit"
        _ <- readComplete s
        return xs

waitUntil :: Handle -> String -> IO a -> IO a
waitUntil handle suffix f = waitUntil' "" where
    suffix' = C.pack suffix
    waitUntil' xs = do
        ready <- hWaitForInput handle 5000
        if ready
        then do
            xs' <- B.concat . ([xs] ++) . (:[]) <$> B.hGetSome handle 4096
            if suffix' `B.isSuffixOf` xs'
            then f
            else waitUntil' xs'
        else waitUntil' xs

readComplete :: (Streams.InputStream B.ByteString, Streams.OutputStream B.ByteString) -> IO String
readComplete (is, _) = C.unpack . snd . stripSuffix "\n" <$> readComplete' "" where
    readComplete' xs = do
        x <- Streams.read is
        if isNothing x
        then return xs
        else let xs' = B.concat [xs, fromJust x]
            in case stripSuffix "\nsparq> " xs' of
                (True, x') -> return x'
                (False, _) -> readComplete' xs'

writeComplete :: (Streams.InputStream B.ByteString, Streams.OutputStream B.ByteString) -> String -> IO ()
writeComplete (_, os) cmd = do
    done <- newEmptyMVar
    _ <- forkIO (Streams.write (Just . C.pack $ cmd ++ "\n") os >> putMVar done ())
    takeMVar done

stripSuffix :: B.ByteString -> B.ByteString -> (Bool, B.ByteString)
stripSuffix a b = if a `B.isSuffixOf` b
    then (True, B.take (B.length b - B.length a) b)
    else (False, b)
