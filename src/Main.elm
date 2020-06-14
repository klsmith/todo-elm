module Main exposing (main)

import Element.Extra
import Todo.App


main : Program () Todo.App.Model Todo.App.Msg
main =
    Element.Extra.document
        { init = Todo.App.init
        , subscriptions = Todo.App.subscriptions
        , update = Todo.App.update
        , view = Todo.App.view
        }
