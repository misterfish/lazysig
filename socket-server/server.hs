{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text, pack, unpack)
import Control.Exception (finally, catch, ErrorCall)
import Control.Monad (forM_, forever, (<=<))
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import Debug.Trace
    ( trace
    )

import Text.Printf
    ( printf
    )

import Hsig
    ( parseInput
    , generate
    )

import Fish
    ( green
    , red
    )

type ClientId = Int
data Client = Client { getId :: ClientId
                     , getConn :: WS.Connection
                     }

data ServerState = ServerState { getClients :: [Client] }

ip :: String
ip = "127.0.0.1"

port :: Int
port = 9160 :: Int

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
    state <- newMVar newServerState
    WS.runServer ip port $ application state

-- | new connection.
application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    let modify'' (s', c', id') = return (s', (c', id'))
    let modify' = modify'' . addClient conn
    (client', id') <- modifyMVar state modify'

    putStrLn . printf "Connecting client with id %s" =<< (green . show) id'

    flip finally (disconnect client' id') $ do
        talk client'
        return () where

    -- | removes it from the clients array, though at the moment that
    -- doesn't change anything (talk loop is already bootstrapped).
    disconnect client'' id'' = do
        putStrLn . printf "Disconnecting client with id %s" =<< (red . show) id''
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

-- uncaught fail/error kills the client connection, but not the server (BTW)

processMsg :: Text -> IO Text
processMsg msg = do
    parsed <- parse msg
    return $ either err pack parsed where
        err = const noParseAnswer

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


(+++) :: Text -> Text -> Text
(+++) = T.append







