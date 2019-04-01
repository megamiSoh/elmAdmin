module Page.MyPageMenu.Info exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Page.Common exposing(..)
import Port as P
import Json.Encode as Encode
import Json.Decode as Decode
import Route exposing (..)
import Page.Common exposing (..)
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder

-- import Regex exposing (Regex)



type alias Model =
    { session : Session
    , check : Bool
    , data : Data
    , page : Int
    , per_page : Int
    , infiniteLoading : Bool
    , screenInfo : ScreenInfo
    , dataList :List DataList
    , checkList : List String
    }

type alias ScreenInfo = 
    { scrollHeight : Int
    , scrollTop : Int
    , offsetHeight : Int}

type alias Data = 
    { data : List DataList 
    , paginate : Paginate }

type alias DataList = 
    { id : Int
    , inserted_at : String
    , is_use : Bool
    , title : String }

type alias Paginate = 
    { end_date : String
    , is_use : Bool
    , page : Int
    , per_page : Int
    , start_date : String
    , title : String
    , total_count : Int
    }

infoEncoder page per_page session = 
    let
        body = 
            Encode.object 
                [ ("page", Encode.int page )
                , ("per_page", Encode.int per_page) ]
                |> Http.jsonBody    
    in
    Api.post Endpoint.infolist (Session.cred session) GetList body (Decoder.infoData Data DataList Paginate)
-- init : Session -> Api.Check ->(Model, Cmd Msg)
init session mobile
    = (
        { session = session
        , page = 1
        , checkList = []
        , infiniteLoading = False
        , per_page = 10
        , dataList = []
        , screenInfo = 
            { scrollHeight = 0
            , scrollTop = 0
            , offsetHeight = 0}
        , data = 
            { data = []
            , paginate = 
                { end_date = ""
                , is_use = False
                , page = 0
                , per_page = 0
                , start_date = ""
                , title = ""
                , total_count = 0
                }
                } 
        ,  check = mobile}
        ,Cmd.batch[ 
            infoEncoder 1 10 session
        ]
    )


scrollEvent msg = 
    on "scroll" (Decode.map msg scrollInfoDecoder)



scrollInfoDecoder =
    Decode.map3 ScreenInfo
        (Decode.at [ "target", "scrollHeight" ] Decode.int)
        (Decode.at [ "target", "scrollTop" ] Decode.int)
        (Decode.at [ "target", "offsetHeight" ] Decode.int)  

subscriptions : Model -> Sub Msg
subscriptions model =
    Api.successId SaveComplete

type Msg 
    = BackBtn
    | GetList (Result Http.Error Data)
    | DetailGo Int
    | SaveComplete Encode.Value
    | PageBtn (Int, String)
    | ScrollEvent ScreenInfo

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                ({model | infiniteLoading = True}, infoEncoder (model.page) model.per_page model.session)
                    -- Nothing ->
                    --     (model, Cmd.none)
                
            else
                (model, Cmd.none)
        PageBtn (idx, str) ->
            case str of
                "prev" ->
                    (model, infoEncoder idx model.per_page model.session)
                "next" ->
                    (model, infoEncoder idx model.per_page model.session)
                "go" -> 
                    (model, infoEncoder idx model.per_page model.session)
                _ ->
                    (model, Cmd.none)
        SaveComplete str ->
            let
                suc = Decode.decodeValue Decode.string str
            in
            case suc of
                Ok ok ->
                   (model, Route.pushUrl (Session.navKey model.session) Route.InfoD) 
            
                Err _->
                    (model, Cmd.none)
        GetList (Ok ok)->
            if ok.data == [] then
            ({model | infiniteLoading = False, checkList = ["empty"]}, Cmd.none)
            else
            ({model | data = ok, dataList = model.dataList ++ ok.data, page = model.page + 1, infiniteLoading = False}, Cmd.none)
        GetList (Err err)->
            (model, Cmd.none)
        DetailGo id ->  
            let
                encodeId = 
                    Encode.string (String.fromInt(id))
            in
            (model, Api.saveId encodeId)
        BackBtn ->
            (model , Route.pushUrl (Session.navKey model.session) Route.MyPage)

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
       if model.check then
       app model
       else 
       web model
      
    }

editorView md textAreaInput readOnly=
        textarea
            [ onInput textAreaInput
            , property "defaultValue" (Encode.string md)
            , class "editor editorStyle"
            , spellcheck False
            , disabled readOnly
            , placeholder "내용을 입력 해 주세요."
            ]
            []


web model= 
    div [ class "container" ]
        [
            commonJustHeader "/image/icon_notice.png" "공지사항",
            contentsBody model,
            pagination 
                PageBtn
                model.data.paginate
        ]
app model = 
    div [class "container"] [
        appHeaderRDetail "공지사항" "myPageHeader whiteColor" Route.MyPage "fas fa-angle-left",
        div ([ class "table scrollHegiht" ] ++ [scrollEvent ScrollEvent])
        [ 
            if List.length (model.data.data) > 0 then
            tbody [class ""] 
            (List.map appContentsBody model.dataList)
            else
            tbody [] [
                tr[] [ 
                    td [] [text "공지사항이 없습니다."]
                ]
            ]
        ,if model.infiniteLoading then
                div [class "loadingPosition"] [
                spinner
                ]
        else
        span [] []
    ]
    ]

appContentsBody item =
    div [class "tableRow",  onClick (DetailGo item.id)] [
        td[class "m_infor_tableCell"][text item.title],
        td[class"notice_date m_infor_notice_date_tableCell"][text (String.dropRight 10 (item.inserted_at))]
    ]

contentsBody model =
    div [ class "info_mediabox" ]
        [ div [ class "table info_yf_table" ]
            [  div [class "tableRow infor_tableRow"]
                    [ div [class "tableCell info_num"]
                        [ text "번호" ]
                    , div [class "tableCell info_title"]
                        [ text "내용" ]
                    , div [class "tableCell info_date"]
                        [ text "등록일" ]
                ]
            , 
            if List.length (model.data.data) > 0 then
            tbody []
                (List.indexedMap (\idx x -> contentsBodyLayout idx x model) model.data.data)
            else
            tbody [] [
                tr[] [ 
                    td [] [text "공지사항이 없습니다."]
                ]
            ]
           

            ]
        ]
contentsBodyLayout idx item model =
        div [ class "tableRow",  onClick (DetailGo item.id)]
            [
                div [class "tableCell info_num_text"] [text (
                    String.fromInt(model.data.paginate.total_count - ((model.data.paginate.page - 1) * 10) - (idx)  )
                )],
                div [class "tableCell info_title_text"] [text item.title],
                div [class "tableCell info_date_text"] [text (String.dropRight 10 (item.inserted_at))]
            ]

