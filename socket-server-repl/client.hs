{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Debug.Trace
    ( trace
    )

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

ip :: String
ip="127.0.0.1"

port :: Int
port=9160

app :: WS.ClientApp () -- IO ()
app conn = do
    putStrLn "Connected!"

    _ <- forkIO . forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    let loop = do
        line <- T.getLine
        unless (T.null line) $ loop << WS.sendTextData conn line

    loop
    WS.sendClose conn ("Doei." :: Text)

main :: IO ()
main = withSocketsDo $ WS.runClient ip port "/" app

(<<) :: IO b -> IO a -> IO b
(<<) = flip (>>)
