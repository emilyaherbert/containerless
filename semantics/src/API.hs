module API where

import Syntax hiding (Env)
import Lib

type Callback = String -> ()

popArg :: State -> (Trace, State)
popArg (State { nextAddr = nextAddr
                , store = store
                , current = current
                , traceContext = traceContext
                , argsStack = argsStack
            }) =
    (a, nextState) where
    (a:args) = argsStack
    nextState = (State {
        nextAddr = nextAddr,
        store = store,
        current = current,
        traceContext = traceContext,
        argsStack = args
    })

newHandler :: State -> Event -> Trace -> Trace -> (Int, State)
newHandler st ev tUri tCb = (0, st)

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
    -- perform request
    let st''' = loadHandler n
    cb resp
    saveHandler n