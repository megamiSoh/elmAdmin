port module Port exposing (..)

import Json.Encode as E
import Json.Decode as Decode
import Route exposing(..)

port checkMobile : () -> Cmd msg
port check : (E.Value -> msg) -> Sub msg

-- subscriptions : Model -> Sub Msg
subscriptions =
    let _ = Debug.log "whatthe" "mymy"
        
    in
        check

            

