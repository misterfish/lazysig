{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)
import Control.Exception (finally, catch, ErrorCall)
import Control.Monad (forM_, forever, (<=<))
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Debug.Trace
    ( trace
    )

import Text.Printf
    ( printf
    )

import Lazysig
    ( parseInput
    , generate
    )

import qualified System.Posix.Signals as SPS

import Fish
    ( green
    , blue
    , cyan
    , yellow
    , red
    )

type Color = String -> IO String

type ClientId = Int

data Client = Client { getId :: ClientId
                     , getConn :: WS.Connection
                     }

data ServerState = ServerState { getClients :: [Client] }

newServerState :: ServerState
newServerState = ServerState []

addClient :: WS.Connection -> ServerState -> (ServerState, Client, ClientId)
addClient conn state = (state', client', id') where
    clients = getClients state
    id' = (+1) . last' $ clients
    last' cls
      | length cls == 0 = -1
      | otherwise = getId . last $ cls
    client' = Client id' conn
    state' = ServerState $ clients ++ [client']

removeClient :: Client -> ServerState -> ServerState
removeClient client state = r' (getId client) (getClients state) where
    r' cid = ServerState . filter ((/= cid) . getId)

main :: IO ()
main = do
    (host, port) <- processArgs =<< getArgs
    state <- newMVar newServerState

    h <- yellow host
    p <- (cyan . show) port
    info $ printf "Listening on host %s port %s" h p

    WS.runServer host port $ application state
    info "Exiting."

processArgs :: ([String]) -> IO (String, Int)
processArgs = p' where
    p' [] = return (defaultHost, defaultPort)
    p' ["-h"] = usageMsg >>= info >> exitSuccess
    p' [arg1] = return (defaultHost, m' arg1)
    p' [arg1, host] = return (host, m' arg1)
    p' _ = usageMsg >>= err >> return ("", 0)
    m' arg1' = maybe error' id $ readMaybe arg1' where
        error' = error $ "Not a number: " ++ arg1'

-- | new connection.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    let modify'' (s', c', id') = return (s', (c', id'))
    let modify' = modify'' . addClient conn
    (client', id') <- modifyMVar state modify'

    info . printf "Connecting client %s" =<< (green . show) id'

    flip finally (disconnect client' id') $ do
        talk client'
        return () where

    -- | removes it from the clients array, though at the moment that
    -- doesn't change anything (talk loop is already bootstrapped).
    disconnect client'' id'' = do
        info . printf "Disconnecting client %s" =<< (red . show) id''
        modifyMVar_ state $ return . removeClient client''
        return state

-- | communication loop.
talk :: Client -> IO ()
talk client = forever $ do
    let conn = getConn client
    msg <- recv' conn
    r <- processMsg msg
    WS.sendTextData conn r where
        recv' :: WS.Connection -> IO Text
        recv' = WS.receiveData

-- uncaught fail/error kills the client connection, but doesn't bring down
-- the server (BTW)

processMsg :: Text -> IO Text
processMsg msg = do
    parsed <- parse msg
    return $ either err' pack parsed where
        err' = const noParseAnswer

parse :: Text -> IO (Either String String)

parse txt = do
    -- | the two steps are different types of either monads, hence the
    -- explicit matching.
    return $ p' (parseInput $ unpack txt) where
        -- | couldn't parse.
        p' (Left _) = Left "bad parse"
        -- | L = invalid state, R = ok generate.
        p' (Right parsed) = generate parsed

noParseAnswer :: Text
noParseAnswer = "✘"

usageMsg :: IO String
usageMsg = do
    return . printf usage =<< getProgName

err :: String -> IO ()
err str = do
    b <- bullet red
    info $ printf "%s Error: %s" b str
    exitFailure

info :: String -> IO ()
info str = do
    b <- bullet blue
    putStrLn $ printf "%s %s" b str
    return ()

usage :: String
usage = "Usage: %s port [host]"

bullet' :: String
bullet' = "٭"

bullet :: Color -> IO String
bullet col = return . printf "%s" =<< col bullet'

defaultHost :: String
defaultHost = "127.0.0.1"

defaultPort :: Int
defaultPort = 9191 :: Int

(+++) :: Text -> Text -> Text
(+++) = T.append







