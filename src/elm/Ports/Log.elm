module Ports.Log exposing (string)

import Json.Encode as Encode
import Ports


string : String -> Cmd msg
string s =
    Ports.send ( "Ports.Log.string", Encode.string s )
