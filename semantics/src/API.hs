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

loadHandler :: Int -> ()
loadHandler n = ()

saveHandler :: Int -> ()
saveHandler n = ()

-- get(response, callback)
get :: State -> Env -> String -> Callback -> ()
get st env resp cb = () where
    (arg, st2) = popArg st
    (arg2, st3) = popArg st2
    (arg3, st4) = popArg st3