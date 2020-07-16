module API where

import Syntax

get :: [Stmt]
get = [
    
    SMeta (MLet "event" BPopArg),
    SMeta (MLet "tUri" BPopArg),
    SMeta (MLet "tCb" BPopArg),
    SMeta (MLet "n" (BNewHandler (TConst (CString "get")) (TId "tUri") (TId "tCb")))
    -- send request

    ];