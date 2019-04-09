module Page.MyPageMenu.MyPost exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port as P
import Json.Decode as Decode
import Json.Encode as Encode
import Route exposing(..)
import Api as Api
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Http as Http

type alias Model 
    = {
        session : Session
        ,checkDevice : String
        , check : Bool
        , page : Int
        , per_page : Int
        , data : DataList
        , infiniteLoading : Bool
        , checkList : List String
        , screenInfo : ScreenInfo
        , dataList : List Data
        , deleteComplete : Bool
        , errAuth : String
        , deleteId : Int
        , loading : Bool
    }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

type alias DataList = 
    { data : List Data
    , paginate : Paginate }

type alias Data = 
    { content : Maybe String
    , id : Int
    , inserted_at : String
    , link_code : String}

type alias Paginate = 
    { inserted_id : Int
    , page : Int
    , per_page : Int
    , total_count : Int }

-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , checkDevice = ""
        , check = mobile
        , page = 1
        , deleteId = 0
        , deleteComplete = False
        , dataList = []
        , checkList = []
        , errAuth = ""
        , loading = True
        , infiniteLoading = False
        , per_page = 10
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , data = 
            { data = []
            , paginate = 
                { inserted_id = 0
                , page = 0
                , per_page = 0
                , total_count = 0 }
            }}
        , mypostList 1 10 session
    )

scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)



scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

mypostList page per_page session = 
    let
        body = 
            Encode.object
                [ ("page" , Encode.int page) 
                , ("per_page" ,Encode.int per_page)]    
                |> Http.jsonBody
    in
    Api.post Endpoint.myPost (Session.cred session) GetList body (Decoder.mypostdata DataList Data Paginate)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.successId SaveId
    , Session.changes GotSession (Session.navKey model.session)]
    -- P.check CheckDevice

type Msg 
    = GetList (Result Http.Error DataList)
    | BackBtn
    | DeletePost Int
    | DeleteComplete (Result Http.Error Decoder.Success)
    | PageBtn (Int, String)
    | ScrollEvent ScreenInfo
    | GetId String
    | SaveId Encode.Value
    | GotSession Session

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check
-- Endpoint.myPostDelete id

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            if model.errAuth == "delete" then
            ({model | session =session},  Api.get DeleteComplete (Endpoint.myPostDelete (String.fromInt(model.deleteId))) (Session.cred session ) (Decoder.resultD))
            else
            ({model | session =session}, mypostList model.page model.per_page session)
        SaveId complete ->
            let
                c = Decode.decodeValue Decode.string complete
            in
            case c of
                Ok ok ->
                    (model, Route.pushUrl (Session.navKey model.session) Route.PostD)
            
                Err _ ->
                    (model, Cmd.none)
        GetId id ->
            let
                encodeId = Encode.string id
            in
            
            (model, Api.saveId encodeId)
        ScrollEvent { scrollHeight, scrollTop, offsetHeight } ->
             if (scrollHeight - scrollTop) <= offsetHeight then
                -- case toInt of
                --     Just val ->
                        -- if (val  < (model.takeList + 10)) then
                        --     ({model | takeList = val, infiniteLoading = False},Cmd.none)
                        -- else 
                if List.length model.checkList > 0 then
                (model, Cmd.none)
                else
                ({model | infiniteLoading = True}, mypostList model.page model.per_page model.session)
                    -- Nothing ->
                    --     (model, Cmd.none)
                
            else
                (model, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    (model, mypostList idx model.per_page model.session)
                "next" ->
                    (model, mypostList idx model.per_page model.session)
                "go" -> 
                    (model, mypostList idx model.per_page model.session)
                _ ->
                    (model, Cmd.none)
        DeleteComplete (Ok ok) ->
            let
                text = Encode.string "삭제 되었습니다."
            in
            
            ({model | deleteComplete = True}, Cmd.batch[mypostList 1 model.per_page model.session, Api.showToast text])
        DeleteComplete (Err err) -> 
            let
                serverErrors = 
                    Api.decodeErrors err    
            in
            
            ({model | errAuth = "delete"}, (Session.changeInterCeptor (Just serverErrors) model.session))
        DeletePost id ->
            ({model | deleteId = id}, Api.get DeleteComplete (Endpoint.myPostDelete (String.fromInt(id))) (Session.cred model.session ) (Decoder.resultD))
        GetList (Ok ok) ->
            if model.deleteComplete then
            ({model | data = ok, dataList = ok.data, page = model.page, deleteComplete = False, loading = False}, Cmd.none)
            else
                if ok.data == [] then
                ({model | infiniteLoading = False, checkList = ["empty"], loading = False}, Cmd.none)
                else
                ({model | data = ok, dataList = model.dataList ++ ok.data, page = model.page + 1, infiniteLoading = False, loading = False}, Cmd.none)
        GetList (Err err) ->
            let
                serverErrors = Api.decodeErrors err
            in
            
            (model, Session.changeInterCeptor (Just serverErrors) model.session) 
        BackBtn ->
            (model, Route.backUrl(Session.navKey model.session) 1)
            

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.check then
            div [] [
                if model.loading then
                div [class "spinnerBack"] [
                    spinner
                    ]
                else 
                div [] []
            , app model
            ]
        else
        web model
    }

justData item =
    case item of
        Just val ->
            val
    
        Nothing ->
            "내용 없음"

web model= 
     div [ class "container" ]
        [
            commonJustHeader "/image/icon_management.png" "나의 게시물관리"
            , contentsBody model.data
            
        ]
app model =
    div[class "container"] [
        appHeaderRDetail "나의 게시물관리" "myPageHeader whiteColor" Route.MyPage "fas fa-angle-left"
        , appcontentsBody model.dataList model.infiniteLoading model.loading
    ]
contentsBody model =
    div [ class "myPost_searchbox" ]
        [ div [ class "myPost_mediabox" ]
            [ 
            if List.length (model.data) > 0 then
            div[] [ul [ class "cbp_tmtimeline" ]
            (List.map contentsLayout model.data)
            
            , pagination 
                PageBtn
                model.paginate]
            else 
            ul [class "m_cbp_tmtimeline"]
                [
                    li [class "noResult"] [text "나의 게시물이 없습니다."]
                ]
        ]
        ]
appcontentsBody model infiniteloading loading=
    div [ class "myPost_searchbox" ]
        [  div [ class "m_myPost_mediabox", scrollEvent ScrollEvent ]
            [ 
                if List.length(model) > 0 then
                ul [ class "m_cbp_tmtimeline" ]
                (List.map appcontentsLayout model)
                else
                ul [class "m_cbp_tmtimeline"]
                [
                    li [class "noResult"] [text "나의 게시물이 없습니다."]
                ]
                ,if infiniteloading then
                    div [class "loadingPosition"] [
                    infiniteSpinner
                    ]
                else
                span [] []
               
        ]
        ]
contentsLayout item= 
    li []
        [ time [ class "cbp_tmtime" ]
            [ span []
                [ text (String.dropRight 10 item.inserted_at) ]
            , span []
                []
            ]
        , div [ class "cbp_tmicon icon1" ]
            [ 
                i [ class "fas fa-video" ]
                []
            ]
        , div [ class "cbp_tmlabel" ]
            [ h2 [onClick (GetId (String.fromInt(item.id)))]
                [ text (justData item.content) ]
            , p []
                [ div [ class "button", onClick (DeletePost item.id) ]
                    [ text "삭제" ]
                ]
            ]
        ]

appcontentsLayout item= 
    li []
        [ time [ class "m_cbp_tmtime" ]
            [ span []
                [ text (String.dropRight 10 item.inserted_at) ]
            , span []
                []
            ]
        , div [ class "m_cbp_tmicon icon1" ]
            [ 
                i [ class "fas fa-video" ]
                []
            ]
        , div [ class "m_cbp_tmlabel" ]
            [ h2 [onClick (GetId (String.fromInt(item.id)))]
                [ text (justData item.content) ]
            , p []
                [ div [ class "button m_mypost_btn",
                onClick (DeletePost item.id) ]
                    [ text "삭제" ]
                ]
            ]
        ]
    
