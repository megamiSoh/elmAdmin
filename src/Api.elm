port module Api exposing (..)



import Api.Endpoint as Endpoint exposing (Endpoint)
-- import Avatar exposing (..)
import Browser
import Browser.Navigation as Nav
import Http 
import Json.Decode as Decode exposing (Decoder, Value, list, decodeString, field, string, bool )
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Url exposing (Url)
import Task exposing (Task)


type Cred
    = Cred String

type Check = 
    Check Bool

credHeader (Cred str) =
    [ Http.header "authorization" ("bearer " ++ str)
    ]

credFormHeader (Cred str) =
    [ Http.header "authorization" ("bearer " ++ str)
    , Http.header "Content-Type" "application/x-www-form-urlencoded"
    ]

credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "token" Decode.string
-- credDecoder : Decoder Cred
checkBrowserDecoder : Decoder Check
checkBrowserDecoder =
    Decode.succeed Check
        |> required "checkBrowser" Decode.bool


credInDecoder (Cred token) =
    Decode.succeed Cred
        |> required "token" Decode.string




-- PERSISTENCE


port onStoreChange : (Value -> msg) -> Sub msg
port params : (Value -> msg) -> Sub msg
port infoCheck : (Value -> msg) -> Sub msg
port saveCheck : (Value -> msg) -> Sub msg
port getInfoParams : (Value -> msg) -> Sub msg
port onSucceesSession : (Value -> msg) -> Sub msg
port successSave : (Value -> msg) -> Sub msg
port receiveKey : (Value -> msg) -> Sub msg
port successId : (Value -> msg) -> Sub msg
port receiveId : (Value -> msg) -> Sub msg
port saveFilter : (Value -> msg) -> Sub msg
port receiveFilter : (Value -> msg) -> Sub msg
port receiveData : (Value -> msg) -> Sub msg
port receive : (Value -> msg) -> Sub msg
port receiveRepeat :(Value -> msg) -> Sub msg
port reReceiveData : (Value -> msg) -> Sub msg
port getsaveFilter : (Value -> msg) -> Sub msg
port videoSuccess : (Value -> msg ) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange viewerDecoder val =
    Decode.decodeValue(viewerDecoder)val
        |> Result.toMaybe


-- storeCredWith : Cred -> Cmd msg
storeCredWith (Cred token)  =
    let _ = Debug.log "storeCredWith"
        json =
            Encode.object
            [ 
            ( "token", Encode.string token )
            ]
    in
    storeCache (Just json)



logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg
port refreshFetchData : () -> Cmd msg
port secRefreshFetch : () -> Cmd msg
port getRefreshToken :  () -> Cmd msg
port getParams : () -> Cmd msg
port saveData : Value -> Cmd msg
port infodata : Value -> Cmd msg
port getInfo : () -> Cmd msg
port saveKey : Value -> Cmd msg
port getKey : () -> Cmd msg
port saveId : Value -> Cmd msg
port getId : () -> Cmd msg
port filter : Value -> Cmd msg
port getfilter : () -> Cmd msg
port sendData : () -> Cmd msg
port toJs : Value -> Cmd msg
port getData : () -> Cmd msg
port reSendData : Value -> Cmd msg
port getSomeFilter : () -> Cmd msg
port deleteData : () -> Cmd msg
port videoData : Value -> Cmd msg
-- port logout : () -> Cmd msg
-- application :
    -- Decoder (Cred -> viewer)
    -- ->
    --     { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
    --     , onUrlChange : Url -> msg
    --     , onUrlRequest : Browser.UrlRequest -> msg
    --     , subscriptions : model -> Sub msg
    --     , update : msg -> model -> ( model, Cmd msg )
    --     , view : model -> Browser.Document msg
    --     }
    -- -> Program Value model msg
type alias Flags = 
    { token : String
    -- , checkBrowser : Bool
    }

flagsDecoder flags = 
    Decode.succeed flags
        |> required "token" string
        -- |> required "checkBrowser" bool
application config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue (field "token" Decode.string) flags
                        |> Result.andThen (Decode.decodeString (credDecoder))
                        |> Result.toMaybe
                checkBrowser =
                    case Decode.decodeValue (field "checkBrowser" Decode.bool) flags of
                        Ok ok ->
                            let _ = Debug.log "flags" ok
                                
                            in
                            
                            ok
                        Err err ->
                            True
                        -- |> Result.withDefault (Check True)
            in
            config.init  maybeViewer checkBrowser  url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


-- storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder  =
    Decode.field "token" credDecoder

-- mobileCheckDecoder =
--     Decode.filed "mobileCheck"

-- HTTP
-- refreshGet msg url maybeCred headerAuth= 
--     Endpoint.request
--         { method = "GET"
--         , url = url
--         , expect = Http.expectJson msg decoder
--         , header = Just headerAuth
--         , body = Http.emptyBody
--         , timeout = Nothing
--         , tracker = Nothing
--         }

-- get : tag ->Endpoint -> Maybe Cred -> Decoder success -> Cmd msg
get msg url maybeCred decoder =
    Endpoint.request
        { method = "GET"
        , url = url
        , expect = Http.expectJson msg decoder
        , headers =
            case maybeCred of
                Just cred ->
                    credHeader cred 

                Nothing ->
                    []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


-- put : Endpoint -> Cred -> (WebData success -> msg ) -> Http.Body -> Decoder success -> Cmd msg
put url cred  tagger body decoder =
    Endpoint.request
        { method = "PUT"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =  credHeader cred 
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }





-- post : Endpoint -> Maybe Cred ->(WebData success -> msg)-> Http.Body -> Decoder success -> Cmd msg
post url maybeCred tagger body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =
            case maybeCred of
                Just cred ->
                     credHeader cred 

                Nothing ->
                    []
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }



    


-- delete : (WebData success ->msg ) -> Endpoint -> Cred -> Http.Body -> Decoder success -> Cmd msg
delete tagger url cred body decoder =
    Endpoint.request
        { method = "DELETE"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers =  credHeader cred 
        , body = body
        , timeout = Nothing
        , tracker = Nothing
        }


-- login : Http.Body -> Decoder (Cred -> a) -> Cmd msg
-- login body decoder msg=
--     post Endpoint.login Nothing msg body (decoderFromCred decoder)

login body msg decoder =
    post Endpoint.login Nothing msg body decoder




-- settings : Cred -> Http.Body -> Decoder (Cred -> a) -> Cmd msg
-- settings cred body decoder msg =
    -- put Endpoint.user cred msg body (Decode.field "user" (decoderFromCred decoder))
-- 
-- testCode = 
--     Decode.field "token" Decode.string
decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


-- testDecode : Decoder (Cred -> a) -> Decoder a
testDecode decoder =
    Decode.map (\fromCred cred -> fromCred cred)
        decoder
        -- credDecoder


-- ERRORS


addServerError : List String -> List String
addServerError list =
    "Server error" :: list


-- decodeErrors : Http.Error -> String
decodeErrors error =
    case error of
        Http.BadStatus response ->
                String.fromInt(response) 
        Http.BadUrl response ->
                response
        Http.Timeout  ->
                "time out"   
        Http.NetworkError ->
                "networkErr"
        Http.BadBody _ ->
                "badbody"

-- testErrorMsg error =
--     case error of
--         Http.BadStatus response ->
--                 if String.fromInt(response) == "401" then
--                     Api.
--         Http.BadUrl response ->
--                 response
--         Http.Timeout  ->
--                 "time out"   
--         Http.NetworkError ->
--                 "networkErr"
--         Http.BadBody _ ->
--                 "badbody"


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors


-- refresh cred msg= 
--     get msg Endpoint.refresh cred credDecoder