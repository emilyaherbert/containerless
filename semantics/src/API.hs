module API where

import Syntax

get :: [Stmt]
get = [
    
    SMeta (MLet "event" TBPopArg),
    SMeta (MLet "tUri" TBPopArg),
    SMeta (MLet "tCb" TBPopArg),
    SMeta (MLet "n" (TBNewHandler (TConst (CString "get")) (TId "tUri") (TId "tCb")))
    -- send request

    ];