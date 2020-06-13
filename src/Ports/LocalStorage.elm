port module Ports.LocalStorage exposing (..)


port saveToLocalStorage :
    { key : String
    , value : String
    }
    -> Cmd msg


port addLocalStorageListener : String -> Cmd msg


port onLocalStorageChange :
    ({ key : String
     , value : Maybe String
     }
     -> msg
    )
    -> Sub msg
