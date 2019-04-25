module Page.MakeExer exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Html.Attributes as Attr
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing (..)
import Route exposing(..)
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
    , saveCheckVal : String
    , sumCount : Int
    , screenInfo : ScreenInfo
    , infiniteLoading : Bool
    , newList : List ListData
    , pageNum : Int
    , show : String
    , contentsId : Int
    }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

type alias GetListData = 
    { data : List ListData
    , paginate: Paginate }

type alias ListData = 
    { difficulty_name : Maybe String
    , duration : String
    , exercise_part_name : Maybe String
    , id : Int
    , inserted_at : String
    , is_use : Bool
    , mediaid : String
    , thembnail : String
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

bodyEncode page perpage title session= 
    let
        list = 
            Encode.object
                [ ("page", Encode.int page)
                , ("per_page", Encode.int perpage)
                , ("title" , Encode.string title)]
        body =
            list
                |> Http.jsonBody
    in
    (Decoder.makeExerList GetListData ListData Paginate)
    |> Api.post Endpoint.makeExerList (Session.cred session) GetData body 
    
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile =
    let
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
        , infiniteLoading = False
        , sumCount = 1
        , title = ""
        , check = mobile
        , saveCheckVal = ""
        , loading = True
        , newList = []
        , contentsId = 0
        , pageNum = 1
        , show = ""
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
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
        [ bodyEncode 1 10 "" session
        , Api.removeJw ()
        ]
    )

type Msg 
    =  GetData (Result Http.Error GetListData)
    | CheckId Int String
    | SaveIdComplete Encode.Value
    | SessionCheck Encode.Value
    | GotSession Session
    | Delete
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | PageBtn (Int, String)
    | OnLoad
    | ScrollEvent ScreenInfo
    | NoOp
    | DeleteConfirm Int
    


toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)

scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int) 


subscriptions : Model -> Sub Msg
subscriptions model=
    Sub.batch [
        Session.changes GotSession (Session.navKey model.session)
        , Api.successId SaveIdComplete
    ]

onLoad msg =
    on "load" (Decode.succeed msg)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteConfirm id ->
            if id == 0 then
            ({model | show = ""}, Cmd.none)
            else
            ({model | contentsId = id, show = "logoutShow"}, Cmd.none)
        NoOp ->
            (model, Cmd.none)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                ({model | infiniteLoading = True}, bodyEncode (model.page)  model.per_page model.title model.session)
                
            else
                (model, Cmd.none)
        OnLoad ->
            if model.sumCount >= List.length(model.getlistData.data) then
            ({model | loading = False}, Cmd.none)
            else
            ({model | sumCount = model.sumCount + 1}, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    ({model | page = idx, pageNum = model.pageNum - 1}, bodyEncode idx model.per_page model.title model.session)
                "next" ->
                    ({model | page = idx, pageNum = model.pageNum + 1}, bodyEncode idx model.per_page model.title model.session)
                "go" -> 
                    ({model | page = idx}, bodyEncode idx model.per_page model.title model.session)
                _ ->
                    (model, Cmd.none)
        DeleteSuccess (Ok ok) ->
            ({model | show = ""}, Cmd.batch [
                bodyEncode model.page model.per_page model.title model.session
                , Api.showToast (Encode.string "삭제되었습니다.")
            ])
        DeleteSuccess (Err err) ->
            ({model | show = ""}, Api.showToast (Encode.string "삭제할 수 없는 게시물입니다."))
        Delete ->
            (model, 
            Decoder.resultD
            |> Api.get DeleteSuccess (Endpoint.makeDelete (String.fromInt (model.contentsId)))(Session.cred model.session)  )
        GotSession session ->
            ({model | session = session}
            , bodyEncode model.page model.per_page model.title session
            )
        SessionCheck check ->
            let
                decodeCheck = Decode.decodeValue Decode.string check
            in
                case decodeCheck of
                    Ok continue ->
                        (model, bodyEncode model.page model.per_page model.title model.session)
                    Err _ ->
                        (model, Cmd.none)
        SaveIdComplete str ->
            if model.saveCheckVal == "" then
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.MakeDetail
            -- Api.historyUpdate (Encode.string "makeExerciseDetail")
            )
            else 
            (model, 
            Route.pushUrl (Session.navKey model.session) Route.TogetherW
            -- Api.historyUpdate (Encode.string "togetherWrite")
            )
        CheckId id str->
            let
                save = Encode.int id
            in
            ({model | saveCheckVal = str},Api.saveId save)
        GetData (Ok ok) -> 
            if model.check then
                if ok.data == [] then
                ({model | getlistData = ok, newList = model.newList ++ ok.data, infiniteLoading = False, loading =False}, Cmd.none)
                else
                ({model | getlistData = ok, page = model.page + 1, newList = model.newList ++ ok.data, infiniteLoading = False, loading = False}, (scrollToTop NoOp))
            else 
                ({model | getlistData = ok, loading =False}, Cmd.none)
        GetData (Err err) -> 
            let
                serverErrors =
                    Api.decodeErrors err
            in  
            (model, Cmd.batch[(Session.changeInterCeptor (Just serverErrors) model.session)
            ])
            
            

view : Model -> {title : String , content : Html Msg}
view model =
    case model.check of
        True ->
            case model.loading of
                True ->
                    { title = "맞춤운동"
                    , content =
                        div [] [
                        a [Route.href Route.MSearch]
                        [appHeaderSearch "맞춤운동" "makeExerHeader"],
                        div [class "spinnerBack"] [
                            spinner
                            ]
                        ] 
                    }
                False ->
                    { title = "맞춤운동"
                    , content =
                        div [] [
                        a [Route.href Route.MSearch]
                        [appHeaderSearch "맞춤운동" "makeExerHeader"],
                        app model
                        , appdeltelayer model
                        ]

                    }
    
        False ->
            if List.length model.getlistData.data > 0  then 
            { title = "맞춤운동"
            , content =
                div [ class "customContainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "/image/icon_customworkout.png" "맞춤운동",
                        bodyContentTitle,deltelayer model,
                        div [ class "customyf_box2"] [
                            div [ class "make_box_title" ]
                                [ h1 [ class "make_yf_h2" ]
                                    [ text "맞춤운동 리스트" ]
                                ],
                            div [] [
                                    div [ class "make_boxwrap" ]
                                    (List.map bodyItem model.getlistData.data)
                            ]

                        ]
                    ]
                     ,pagination
                    PageBtn
                    model.getlistData.paginate
                    model.pageNum
                ]
            ]
            }
            else
                { title = "맞춤운동"
                , content =
                    div [ class "customContainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "/image/icon_customworkout.png" "맞춤운동",
                        bodyContentTitle,deltelayer model,
                        div [ class "customyf_box2"] [
                            div [ class "make_box_title" ]
                                [ h1 [ class "make_yf_h2" ]
                                    [ text "맞춤운동 리스트" ]
                                ],
                            div [] [
                                    div [class "noResult"] [text "맞춤영상이 없습니다."]
                            ]

                        ]
                    ]
                ]
            ]
                }
        


web model = 
    div [ class "customContainerwrap" ]
            [ div [ class "container" ]
                [ div [ class "notification yf_workout" ]
                    [
                        commonHeader "/image/icon_customworkout.png" "맞춤운동",
                        bodyContentTitle,deltelayer model,
                        div [ class "customyf_box2"] [
                            div [ class "make_box_title" ]
                                [ h1 [ class "make_yf_h2" ]
                                    [ text "맞춤운동 리스트" ]
                                ],
                            div [] [
                                 if List.length model.getlistData.data > 0 then
                                    div [ class "make_boxwrap" ]
                                    (List.map bodyItem model.getlistData.data)
                                else
                                    div [class "noResult"] [text "맞춤영상이 없습니다."]
                            ]

                        ]
                    ]
                     ,pagination
                    PageBtn
                    model.getlistData.paginate
                    model.pageNum
                ]
            ]
app model =
    div [ class "container", class "scroll", scrollEvent ScrollEvent, style "height" "85vh" ][
         appStartBox
        , listTitle
        -- ,if List.length model.newList > 0 then
            ,div [] [
                div[](List.map appItemContent model.newList) ,
                if model.infiniteLoading then
                    div [class "loadingPosition"] [
                    infiniteSpinner
                    ]
                else
                    span [] []
            ]
        -- else
        -- div [class "noResult"] [text "맞춤영상이 없습니다."]
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
    let
        titleReplace = 
            item.title 
                |> String.replace "%26" "&"
                |> String.replace "%25" "%"    
    in
    
        div [ class "m_make_yf_box2" ]
            [ div [ class "m_make_videoimg", onClick (CheckId item.id "") ]
                [ img [ src item.thembnail, onLoad OnLoad ]
                    []
                ]
      
            , div [ class "m_make_yf_box_title", onClick (CheckId item.id "") ]
                [ text (
                    if String.length titleReplace > 10 then
                    String.left 10 titleReplace ++ "..."
                    else
                    titleReplace
                ) ]
            , div [ class "make_yf_ul" ]
                [ ul []
                    [ li []
                        [ text (String.dropRight 10 (item.inserted_at)) ]
                    , li [] [
                        i [ class "fas fa-stopwatch" ]
                        []
                        , text " "
                        , text item.duration
                    ]
                    ]
                ]
            , div [ class "button is-dark m_makeExercise_share"
            , onClick (CheckId item.id "share")
            ]
                [ i [ class "fas fa-share-square" ]
                [], text "공유하기" 
            ]

                , div [ class "button m_makeExercise_dete",onClick (DeleteConfirm item.id) ]
                [ i [ class "far fa-trash-alt" ]
                [], text "삭제" 
            ]
            ]

bodyItem item=
    let
        titleReplace = 
            item.title 
                |> String.replace "%26" "&"
                |> String.replace "%25" "%"    
    in
    div [ class "make_box_card_wrap" ]
    [ div [ class "make_videoboxwrap"]
        [ div [ class "video_image" , onClick (CheckId item.id "")]
            [ img [ class "vpic1",src item.thembnail, alt "dummy_video_image" ]
                []
            ]
        , div [ class "Customtextbox"]
            [ div [ class "m1"  , onClick (CheckId item.id "")]
                [ h1 [ class "make_yf_titlename" ]
                    [ text  (
                    if String.length titleReplace > 10 then
                    String.left 10 titleReplace ++ "..."
                    else
                    titleReplace
                ) ]
                ]
            , div [ class "m2" ]
                [ text (String.dropRight 10 (item.inserted_at)), 
                div [] [
                    i [ class "fas fa-stopwatch" ]
                        []
                        , text " "
                        , text item.duration
                ]
                , p [class "makebtn"]
                    [ div  [ class "button is-dark darkbtn make_share"
                    , onClick (CheckId item.id "share") ]
                        [ i [ class "fas fa-share-square" ]
                            [] , text "공유" 
                        ]
                    , div [ class "button" ,onClick (DeleteConfirm item.id)]
                        [ i [ class "far fa-trash-alt" ]
                            [] , text "삭제" 
                        ]
                    ]
                ]
            ]
        ]
    ]




bodyContentTitle =
          div [ class "make_yf_box" ] 
        
                [ 
            img [ src "image/makeimage.png", alt "makeimage" ]
                []
           ,
                    h1 [ class "make_yf_h1" ]
                [ text "하나뿐인 나만의 운동을 만들어보세요!" ]
             , a [ class "button is-dark make_yf_darkbut", Route.href Route.Filter ]
                [ text "시작하기" ]
            , br []
                []
            ]

appdeltelayer model =
    div [class ("m_delete_post " ++ model.show)] [
         div [ class "yf_delete_popup" ]
            [ h1 [ class "popup_yf" ]
                [ text "게시물을 삭제하시겠습니까?" ]
            , p [ class "yf_logout_butbox" ]
                [ div [ class "button is-light logout_danger2", onClick (DeleteConfirm 0) ]
                    [ text "취소" ]
                , div [ class "button is-danger logout_cencel2", onClick Delete ]
                    [ text "삭제" ]
                ]
            ]
    ]

deltelayer model =
    div [class ("delete_post " ++ model.show)] [
         div [ class "yf_delete_popup" ]
            [ h1 [ class "popup_yf" ]
                [ text "게시물을 삭제하시겠습니까?" ]
            , p [ class "yf_logout_butbox" ]
                [ div [ class "button is-light logout_danger2", onClick (DeleteConfirm 0) ]
                    [ text "취소" ]
                , div [ class "button is-danger logout_cencel2", onClick Delete ]
                    [ text "삭제" ]
                ]
            ]
    ]