port module Port exposing (..)

import Json.Encode as E
import Json.Decode as Decode
import Route exposing(..)

port checkMobile : () -> Cmd msg
port check : (E.Value -> msg) -> Sub msg

subscriptions :  (E.Value -> msg ) -> Sub msg
subscriptions =
        check
