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

newHandler :: Event -> Trace -> Trace -> Int
newHandler ev tUri tCb = 0

loadHandler :: Int -> ()
loadHandler n = ()

saveHandler :: Int -> ()
saveHandler n = ()

-- get(response, callback)
get :: State -> Env -> String -> Callback -> ()
get st env resp cb = () where
    (State { nextAddr=nextAddr, store=store, current=current, traceContext=traceContext, argsStack=argsStack }) = st
    (_:tUri:tCb:args) = argsStack