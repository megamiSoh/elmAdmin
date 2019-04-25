module TextApi exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Json.Encode as Encode
import Http as Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, required, hardcoded, optional)
import Html.Events exposing (..)
import Session exposing(..)
---- MODEL ----

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

type alias Model =
    { session : Session
    , check : Bool
    , postData : TogetherDataWrap 
    , page : Int
    , per_page : Int}

mypostDataWrap = 
    Decode.succeed TogetherDataWrap
        |> required "data" (Decode.list togetherdata)
        |> required "paginate" paginate

togetherdata =
    Decode.succeed TogetherData
        |> optional "content" (Decode.map Just string) Nothing
        |> optional "detail" (Decode.map Just (Decode.list detailTogether )) Nothing
        |> required "id" int
        |> required "inserted_at" string 
        |> required "is_delete" bool
        |> required "link_code" string
        |> required "recommend_cnt" int
        |> optional "nickname" (Decode.map Just string) Nothing
        |> optional "profile" (Decode.map Just string) Nothing


detailTogether = 
    Decode.succeed DetailTogether 
        |> required "thembnail" string
        |> optional "difficulty_name" (Decode.map Just string) Nothing
        |> required "duration" string
        |> required "exercise_items" (Decode.list togetherItem)
        |> optional "exercise_part_name" (Decode.map Just string) Nothing
        |> required "id" int
        |> required "inserted_at" string
        |> required "pairing" (Decode.list pairing)
        |> required "title" string


togetherItem  = 
    Decode.succeed TogetherItems
        |> required "exercise_id" int
        |> required "is_rest" bool
        |> required "sort" int
        |> required "title" string
        |> required "value" int
pairing = 
    Decode.succeed Pairing
        |> required "file" string
        |> required "image" string
        |> required "title" string

paginate  = 
    Decode.succeed Paginate
        |> required "page" int
        |> required "per_page" int
        |> required "total_count" int


post : Int -> Int -> String -> Cmd Msg
post page perpage url=
    let
        togetherBody =
            Encode.object 
                [ ("page" , Encode.int page)
                , ("per_page", Encode.int perpage)]
                |> Http.jsonBody
    in
    
    Http.post 
        { url = url
        , body = togetherBody
        , expect = Http.expectJson GotData mypostDataWrap }

type alias TogetherDataWrap = 
    { data : List TogetherData 
    , paginate : Paginate
    }


    
type alias TogetherData = 
    { content : Maybe String
    , detail : Maybe (List DetailTogether)
    , id : Int
    , inserted_at : String
    , is_delete : Bool
    , link_code : String
    , recommend_cnt : Int
    , nickname : Maybe String
    , profile : Maybe String
    }
type alias DetailTogether = 
    { thembnail : String
    , difficulty_name : Maybe String
    , duration : String
    , exercise_items : List TogetherItems
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , pairing : List Pairing 
    , title : String
    }
type alias TogetherItems = 
    { exercise_id : Int
    , is_rest : Bool
    , sort : Int
    , title : String
    , value : Int }
type alias Pairing = 
    { file : String
    , image : String
    , title : String 
    }
type alias Paginate = 
    { page : Int
    , per_page : Int
    , total_count : Int }


init : Session -> Bool -> ( Model, Cmd Msg )
init session mobile =
    ( { postData = 
        { data = []
        , paginate = 
            { page = 1
            , per_page = 10
            , total_count = 0
            }
        }
        , page = 1
        , per_page = 10
        , session = session
        , check = mobile

        }, Cmd.none )



---- UPDATE ----


type Msg
    = GotData (Result Http.Error TogetherDataWrap)
    | GoDevPost
    | GoPost
    | PerPage String
    | Page String

toInt item = 
    case item of
        Just int ->
            int
    
        Nothing ->
            0

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PerPage int->
            let
              toint = String.toInt int
            in
            ({model | per_page = toInt toint}, Cmd.none)
        Page int->
            let
              toint = String.toInt int
            in
            ({model | page = toInt toint}, Cmd.none)
        GotData (Ok ok) ->
            ( {model | postData = ok}, Cmd.none )
        GotData (Err err)->
            ( model, Cmd.none )
        GoDevPost ->
            (model, post model.page model.per_page "http://13.209.49.169:4000/api/v1/front/together/test")
        GoPost ->
            (model, post model.page model.per_page "http://13.209.49.169:4000/api/v1/front/together/test")
            



---- VIEW ----


view : Model -> {title : String , content : Html Msg}
view model =
    { title = "text"
    , content = 
        div []
            [ 
                div [][
                    text "per_page : "
                    , input 
                    [ onInput PerPage
                    , Attr.value (String.fromInt model.per_page)] []
                ]
                , br [] []
                , div [] [
                    text "page : "
                    , input [onInput Page
                    , Attr.value (String.fromInt model.page) ][]
                ]
                , br [] []
                , button [onClick GoDevPost][text "Dev together Call"]
                , br [] []
                , br [] []
                , button [onClick GoPost][text "together Call"]
                , br [] []
                , div [][
                    text (String.fromInt(model.postData.paginate.total_count))
                ]
            ]
        }
