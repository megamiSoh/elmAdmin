module Page.Detail.ContactDetail exposing (..)
import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Json.Decode as Decode
import Json.Encode as E
import Route exposing (..)
import Page.Common exposing (..)
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder 


type alias Model 
    = {
        session : Session
        , checkDevice : String
        , check : Bool
        , getId : String
        , detail : Detail
        , edit : Bool
    }

type alias Data = 
    { data : Detail }

type alias Detail = 
    { answer : Maybe String
    , asked_id : Int
    , content : String
    , id : Int
    , is_answer : Bool
    , title : String
    , username : String }

detailApi id session msg= 
    Api.get msg (Endpoint.faqDetail id) (Session.cred session) (Decoder.faqdetail Data Detail)

editEncoder title content session id msg =
    let
        new string = 
            string
                |> String.replace "&" "%26" 
                |> String.replace "%" "%25"
        body =
            ("title=" 
                ++ (new title)
                ++ "&content="
                ++ (new content)
            )
            |> Http.stringBody "application/x-www-form-urlencoded"
    in
    Api.post (Endpoint.faqeidt id)(Session.cred session) msg body (Decoder.resultD)
    
init : Session -> Bool ->(Model, Cmd Msg)
init session mobile = 
    (
        {session = session
        ,checkDevice = ""
        , check = mobile
        , getId = ""
        , edit = True
        , detail = 
            { answer = Nothing
            , asked_id = 0
            , content = ""
            , id = 0
            , is_answer = False
            , title = ""
            , username = "" }
        }
        , Cmd.batch[Api.getKey ()
        , scrollToTop NoOp]
    )

type Msg 
    = CheckDevice E.Value
    | GetId E.Value
    | GetDetail (Result Http.Error Data)
    | GoDelete
    | DeleteSuccess (Result Http.Error Decoder.Success)
    | GoEdit
    | ChangeEdit
    | Title String
    | Content String
    | EditComplete(Result Http.Error Decoder.Success)
    | GotSession Session
    | GoBack
    | NoOp
    | ClickRight
    | ClickLeft
    | GoAnotherPage

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch[Api.receiveKey GetId
    , Session.changes GotSession (Session.navKey model.session)
    ]

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        old = model.detail

        recordsUpdateTitle result = 
            {old | title = result}

        recordsUpdateContent result = 
            {old | content = result}
    in
    
    case msg of
        GoAnotherPage ->
            (model, Cmd.batch [
                 Api.setCookie (E.int 1)
            ])
        ClickRight ->
            ( model, Api.scrollRight () )
        ClickLeft ->
            (model , Api.scrollLeft ())
        NoOp ->
            (model, Cmd.none)
        GoBack ->
            (model, Route.pushUrl(Session.navKey model.session) Route.C)
        GotSession session ->
            ({model | session = session},detailApi model.getId session GetDetail)
        EditComplete (Ok ok) ->
            if model.check then
            (model, Cmd.batch[
                Api.showToast (E.string "수정이 완료 되었습니다.")
                , Route.pushUrl (Session.navKey model.session) Route.C
            ])
            else
            ({model | edit = not model.edit}, Api.showToast (E.string "수정이 완료 되었습니다."))
        EditComplete (Err err) ->
            (model, Cmd.none)
        Title title ->
            ({model | detail = (recordsUpdateTitle title) } , Cmd.none)
        Content content ->
            ({model | detail = (recordsUpdateContent content)}, Cmd.none)
        ChangeEdit ->
            ({model | edit = not model.edit}, scrollToTop NoOp)
        GoEdit ->
            (model , Cmd.batch[editEncoder model.detail.title model.detail.content model.session model.getId EditComplete
            , scrollToTop NoOp])
        DeleteSuccess (Ok ok) ->
            (model, Cmd.batch[Api.showToast (E.string "문의가 삭제되었습니다.")
            , Route.pushUrl (Session.navKey model.session) Route.C])
        DeleteSuccess (Err err) ->
            (model, Api.showToast (E.string "문의를 삭제 할 수 없습니다."))
        GoDelete ->
            (model, Api.get DeleteSuccess (Endpoint.faqDelete model.getId) (Session.cred model.session) (Decoder.resultD) )
        GetDetail (Ok ok) ->
            ({model | detail = ok.data}, Cmd.none)
        GetDetail (Err err) ->
            let 
                serverErrors = Api.decodeErrors err
            in
            if serverErrors == "401" then
            (model, (Session.changeInterCeptor(Just serverErrors)model.session))
            else
            (model, Cmd.none)
        GetId id ->
            let
                get = Decode.decodeValue Decode.string id
            in
            case get of
                Ok ok ->
                    ({model | getId = ok}, detailApi ok model.session GetDetail)
                Err err ->
                    (model, Cmd.none)
        CheckDevice str ->
            let
                result = Decode.decodeValue Decode.string str
            in
                case result of
                    Ok string ->
                        ({model | checkDevice = string},Cmd.none)
                
                    Err _ ->
                        ({model | checkDevice = ""},Cmd.none)
                        
            

view : Model -> {title : String , content : Html Msg}
view model =
    -- if model.check then
    -- { title = "YourFitExer"
    -- , content = 
    --     div [] [
    --     app model
    -- ]}
    -- else
    { title = "YourFitExer"
    , content = 
        div [] [
        div[][myPageCommonHeader ClickRight ClickLeft GoAnotherPage False]
        , web model
    ]}
    

justString item = 
    case item of
        Just ok ->
            decodeChar ok
    
        Nothing ->
            "등록된 답변이 없습니다."

web model =
    div [class "container"] [
        commonJustHeader "/image/icon_qna.png" "1:1문의",
        ques model.detail model.edit,
        answer model.detail,
        backBtn
    ]



ques item edit = 
        div [ class "info_mediabox contentsH" ]
            [ div [ class "infoDetail_titlebox" ]
                [ div []
                    [ input [class "infoDetail_titleweb", value (decodeChar item.title), disabled edit, maxlength 50, onInput Title] [] ,
                    if item.is_answer then
                        span [class "faqisAnswer"][text "답변완료"]
                    else
                        span [class "faqisAnswer red"][text "답변 대기 중"]
                    ]
                ]
            , textarea [ class "infoDetail_textboxweb" , value (decodeChar item.content), disabled edit, maxlength 250, onInput Content]
                []
            , if item.is_answer then
            div [] []
            else
            p [class"faqDetail_btnbox"]
                [ 
                    if edit then
                    div [ class "button faqDetail_edit" , onClick ChangeEdit]
                    [ 
                        i [ class "fas fa-edit" ]
                        [], text "수정" 
                    ]
                    else
                    div [ class "button faqDetail_edit" , onClick GoEdit]
                    [ 
                        i [ class "fas fa-edit" ]
                        [], text "저장" 
                    ]
                    , div [ class "button" , onClick GoDelete]
                    [ i [ class "far fa-trash-alt" ]
                        [], text "삭제" 
                    ]
                ]
            ]

answer item= 
    div [ class "info_mediabox contentsH" ]
        [ div [ class "infoDetail_titlebox", style "background" "#e4e4e4" ]
            [ div [ class "infoDetail_titleweb" ]
                [ text ("Re : " ++ (decodeChar item.title)) ]
            ]
        , div [ class "infoDetail_textboxweb" ]
            [text (justString item.answer)]
            ]
backBtn =
    div [ class "make_yf_butbox" ]
    [ a [ class "button infoDetail_yf_back", Route.href Route.C ]
        [ text "뒤로" ]
    ]       

-- app model = 
--     div [class "container"] [
--         appHeaderConfirmDetailleft "문의 하기" "myPageHeader" GoBack GoEdit "수정" 
--         , apptitle model.detail.title
--         , if model.detail.is_answer then
--             div [class "appFaqleft"][text "답변완료"]
--         else
--             div [class "appFaqleft red"][text "답변 대기 중"]
--         , apptextArea model.detail.content
--     ]

apptitle title titleInput = 
        input [ class "input", type_ "text", placeholder "제목을 입력해주세요" , maxlength 50, onInput titleInput , value (decodeChar title) ]
                []
apptextArea content contentInput =
        textarea [ class "textarea", placeholder "내용을 입력해주세요", rows 10, maxlength 250 , onInput contentInput, value (decodeChar content)]
        []


            
decodeChar char = 
    char
        |> String.replace  "%26" "&"
        |> String.replace  "%25" "%"

