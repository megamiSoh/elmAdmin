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
import Browser.Dom as Dom

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
port screenInfo : (Value -> msg) -> Sub msg
port getPageId : (Value -> msg) -> Sub msg
port setCookieSuccess : (Value -> msg) -> Sub msg
port getHeightValue : (Value -> msg) -> Sub msg
port videoWatchComplete : (Value -> msg) -> Sub msg
port sendHeight : (Value -> msg) -> Sub msg
port touch : (Value -> msg) -> Sub msg
port progressComplete : (Value -> msg) -> Sub msg
port calcurationComplete : (Value -> msg) -> Sub msg
port dateValidResult : (Value -> msg) -> Sub msg
port hideThum : (Value -> msg) -> Sub msg
port sliderRestart : (Value -> msg) -> Sub msg
port autoSlide : (Value -> msg) -> Sub msg
port transitionCheck : (Value -> msg) -> Sub msg
port swipe : (Value -> msg) -> Sub msg
port commaF : (Value -> msg) -> Sub msg
port receivetogetherId : (Value -> msg) -> Sub msg
port reOpenFile : (Value -> msg) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange viewerDecoder val =
    Decode.decodeValue(viewerDecoder)val
        |> Result.toMaybe


storeCredWith (Cred token)  =
    let
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

port callScreen : () -> Cmd msg
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
port togetherDataList : Value -> Cmd msg
port showToast : Value -> Cmd msg
port blur : () -> Cmd msg
port scrollRight : () -> Cmd msg
port scrollLeft : () -> Cmd msg
port scrollR : Value -> Cmd msg
port scrollL : Value -> Cmd msg
port expand : () -> Cmd msg
port removeId : () -> Cmd msg
port setCookie : Value -> Cmd msg
port getCookie : () -> Cmd msg
port getscrollHeight : Value -> Cmd msg
port backUrl : () -> Cmd msg
port historyUpdate : Value -> Cmd msg
port removeJw : () -> Cmd msg
port logoutpop : () -> Cmd msg
port scrollControl : () -> Cmd msg
port getHeight : Value -> Cmd msg
port mypageMenu : Value -> Cmd msg
port bodyInfo : () -> Cmd msg
port hideFooter : () -> Cmd msg
port progressGo : () -> Cmd msg
port progressCalcuration : () -> Cmd msg
port valueReset : Value -> Cmd msg
port dateValidate : Value -> Cmd msg
port youtubeVideo : Value -> Cmd msg
port slide : Value -> Cmd msg
port payment : Value -> Cmd msg
port mobilePaymentCheck : () -> Cmd msg
port comma : Value -> Cmd msg
port openPop : () -> Cmd msg
port hamburgerShut : () -> Cmd msg
port togetherId : () -> Cmd msg
port openFile : () -> Cmd msg
type alias Flags = 
    { token : String
    }

unfocus noop =
    Task.attempt(\_ -> noop) (Dom.blur "keyboardBlur")

flagsDecoder flags = 
    Decode.succeed flags
        |> required "token" string

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
                            ok
                        Err err ->
                            True
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


storageDecoder  =
    Decode.field "token" credDecoder

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

noSessionpost url tagger body decoder =
    Endpoint.request
        { method = "POST"
        , url = url
        , expect = Http.expectJson tagger decoder
        , headers = []
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



login body msg decoder =
    post Endpoint.login Nothing msg body decoder


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


testDecode decoder =
    Decode.map (\fromCred cred -> fromCred cred)
        decoder


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




errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors

