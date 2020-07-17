module API where

import Syntax hiding (Env)
import EvalJSEnv

type Callback = String -> ()

loadHandler :: Int -> ()
loadHandler n = ()

saveHandler :: Int -> ()
saveHandler n = ()

-- get(response, callback)
get :: State -> Env -> String -> Callback -> ()
get st env resp cb = ()