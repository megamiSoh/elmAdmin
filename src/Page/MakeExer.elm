module Page.MakeExer exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Html.Attributes as Attr
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing(..)
import Port as P
import Json.Encode as Encode
import Json.Decode as Decode
import Http as Http exposing(..)
import Api as Api
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
type alias Model = 
    { session : Session
    , checkDevice : String
    , page : Int
    , per_page : Int
    , title : String
    , getlistData : GetListData
    , check : Bool
    , loading : Bool
    }

-- type State = 
--     Loading | Ready

type alias GetListData = 
    { data : List ListData
    , paginate: Paginate }

type alias ListData = 
    { difficulty_name : Maybe String
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , is_use : Bool
    , title : String
    }

type alias Paginate = 
    { difficulty_code : String
    , end_date : String
    , exercise_part_code : String
    , inserted_id : Int
    , make_code : String
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    }

bodyEncode model session= 
    let
        list = 
            Encode.object
                [ ("page", Encode.int model.page)
                , ("per_page", Encode.int model.per_page)
                , ("title" , Encode.string model.title)]
        body =
            list
                |> Http.jsonBody
    in
    Api.post Endpoint.makeExerList (Session.cred session) GetData body (Decoder.makeExerList GetListData ListData Paginate)
    
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
    let _ = Debug.log "session" mobile
        listmodel = 
            { page = 1
            , per_page = 10
            , title = ""}
    in
    (
        { session = session
        , checkDevice = ""
        , page = 1
        , per_page = 10
        , title = ""
        , check = mobile
        , loading = True
        , getlistData = 
            { data = []
            , paginate =
                { difficulty_code = ""
                , end_date = ""
                , exercise_part_code = ""
                , inserted_id = 0
                , make_code = ""
                , page = 0
                , per_page = 0
                , start_date = ""
                , title = ""
                , total_count = 0
                }
            }
        }, 
        Cmd.batch 
        [ P.checkMobile ()
        , bodyEncode listmodel session
        ]
    )

type Msg 
    =  GetData (Result Http.Error GetListData)
    | CheckId Int
    | SaveIdComplete Encode.Value
    | SessionCheck Encode.Value
    | GotSession Session
    | Delete Int
    | DeleteSuccess (Result Http.Error Decoder.Success)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


subscriptions : Model -> Sub Msg
subscriptions model=
    Sub.batch [
        Session.changes GotSession (Session.navKey model.session)
        , Api.successId SaveIdComplete
        , Api.onSucceesSession SessionCheck
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteSuccess (Ok ok) ->
            (model, bodyEncode model model.session)
        DeleteSuccess (Err err) ->
            (model, Cmd.none)
        Delete id ->
            (model, Api.get DeleteSuccess (Endpoint.makeDelete (String.fromInt (id)))(Session.cred model.session) Decoder.resultD )
        GotSession session ->
            ({model | session = session}
            , Cmd.none
            )
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, bodyEncode model model.session)
                    Err _ ->
                        (model, Cmd.none)
        SaveIdComplete str ->
            (model, Route.pushUrl (Session.navKey model.session) Route.MakeDetail)
        CheckId id ->
            let
                save = Encode.int id
            in
            (model,Api.saveId save)
        GetData (Ok ok) -> 
            ({model | getlistData = ok, loading = False}, Cmd.none)
        GetData (Err err) -> 
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, (Session.changeInterCeptor (Just serverErrors) model.session))
        

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content =
        webOrApp model

    }
webOrApp model= 
    if model.check then
         div [] [appHeaderSearch "맞춤운동" "makeExerHeader",
            if model.loading then
            spinner 
            else
            app model]  
    else 
        web model

web model = 
    div [ class "customContainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "../image/icon_customworkout.png" "맞춤운동",
                        bodyContentTitle,
                        div [ class "customyf_box2"] [
                            div [ class "make_box_title" ]
                                [ h1 [ class "make_yf_h2" ]
                                    [ text "맞춤운동 리스트" ]
                                ],
                            if model.loading then
                                spinner
                            else
                                    if List.length model.getlistData.data > 0 then
                                    div [ class "make_boxwrap" ]
                                    (List.map bodyItem model.getlistData.data)
                                else
                                    div [] [text "맞춤영상이 없습니다."]

                        ]
                    ]
                ]
                ,pagenation
            ]
app model =
    div [ class "container" ][
         appStartBox
        ,listTitle
        ,div[](List.map appItemContent model.getlistData.data)
    ]

appStartBox = 
    div [ class "make_m_yf_box" ]
        [ h1 [ class "m_make_yf_h1" ]
            [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
        , a [ class "button is-dark m_make_yf_darkbut", Route.href Route.Filter ]
            [ text "시작하기" ]
        , br []
            []
        ]
listTitle = 
    div [ class "m_make_box_title" ]
        [ h1 [ class "m_make_yf_h2" ]
            [ text "맞춤운동 리스트" ]
        ]

appItemContent item=
        div [ class "m_make_yf_box2" ]
            [ div [ class "m_make_videoimg", onClick (CheckId item.id) ]
                [ img [ src "/image/dummy_video_image3.png" ]
                    []
                ]
      
            , div [ class "m_make_yf_box_title", onClick (CheckId item.id) ]
                [ text item.title ]
            , div [ class "make_yf_ul" ]
                [ ul []
                    [ li [class"m_make_share"]
                        [ text item.inserted_at ]
          
                    , a [ class "button is-dark m_makeExercise_share" ]
                        [ i [ class "fas fa-share-square" ]
                        [], text "공유하기" 
                    ]

                      , div [ class "button m_makeExercise_dete",onClick (Delete item.id) ]
                        [ i [ class "far fa-trash-alt" ]
                        [], text "삭제" 
                    ]
                        
                    ]
                ]
            ]

bodyItem item=
    div [ class "make_box_card_wrap" ]
    [ div [ class "make_videoboxwrap"]
        [ div [ class "video_image" , onClick (CheckId item.id)]
            [ img [ class "vpic1", src "/image/dummy_video_image3.png", alt "dummy_video_image" ]
                []
            ]
        , div [ class "Customtextbox"]
            [ div [ class "m1"  , onClick (CheckId item.id)]
                [ h1 [ class "make_yf_titlename" ]
                    [ text item.title ]
                ]
            , div [ class "m2" ]
                [ text item.inserted_at,  p [class "makebtn"]
                    [ a [ class "button is-dark darkbtn make_share" ]
                        [ i [ class "fas fa-share-square" ]
                            [] , text "공유" 
                        ]
                    , div [ class "button" ,onClick (Delete item.id)]
                        [ i [ class "far fa-trash-alt" ]
                            [] , text "삭제" 
                        ]
                    ]
                ]
            ]
        ]
    ]

pagenation = 
    div [ class "customyf_Pagination" ]
        [ nav [ class "pagination is-centered" ]
            [ ul [ class "pagination-list" ]
                [ li [ class "" ]
                    [ a [ class "pagination-link"]
                        [ text "<" , text "<" ]
                    ]
                , a [ class "pagination-link"]
                    [ text "<" ]
                , li []
                    [ a [ class "pagination-link is-current yf_cut" ]
                        [ text "5" ]
                    ]
                , li []
                    [ a [ class "pagination-link" ]
                        [ text ">" ]
                    ]
                , a [ class "pagination-link" ]
                    [ text ">>" ]
                ]
            ]
        ]


bodyContentTitle =
          div [ class "make_yf_box" ] 
        
                [ 
            img [ src "image/runimage.png", alt "runimage" ]
                []
           ,
                    h1 [ class "make_yf_h1" ]
                [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
             , a [ class "button is-dark make_yf_darkbut", Route.href Route.Filter ]
                [ text "시작하기" ]
            , br []
                []
            ]
