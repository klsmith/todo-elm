module Main exposing (..)

import Element.Extra
import Todo.App


main =
    Element.Extra.document
        { init = Todo.App.init
        , subscriptions = Todo.App.subscriptions
        , update = Todo.App.update
        , view = Todo.App.view
        }
