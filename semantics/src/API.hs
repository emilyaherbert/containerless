module API where

import Syntax hiding (Env)
import Lib

type Callback = String -> Int

popArg :: State -> (Trace, State)
popArg st = (a, st {argsStack = args}) where
    (a:args) = argsStack st

newHandler :: State -> Event -> Trace -> Trace -> (Int, State)
newHandler st ev tUri tCb = (0, st') where
    (n, st') = freshEventAddr st

loadHandler :: Int -> ()
loadHandler n = ()

saveHandler :: Int -> ()
saveHandler n = ()

-- get(response, callback)
getImmediate :: State -> Env -> String -> Callback -> ()
getImmediate st env resp cb = do
    let (State { nextAddr=nextAddr, store=store, current=c, traceContext=k, argsStack=a }) = st
    let (_:tUri:tCb:a') = a
    let (n, st') = newHandler (st {argsStack=a'}) EvGet tUri tCb
    -- get request is sent here
    let st''' = loadHandler n
    let x = cb resp
    saveHandler n