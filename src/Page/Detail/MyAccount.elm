module Page.Detail.MyAccount exposing (..)

import Browser exposing (..)
import Html.Events exposing(..)
import Html.Attributes exposing(..)
import Session exposing(..)
import Html exposing (..)
import Port exposing(..)
import Page.Common exposing(..)
import Route exposing (..)
import Api as Api
import Http as Http
import Api.Endpoint as Endpoint
import Api.Decoder as Decoder
import Page.MyPage as My
import Json.Decode as Decode
import Json.Encode as Encode

type alias Model 
    = {
        session : Session
        , check : Bool
        , mydata : MyData
        , currentPage : String
        , nickname : String
    }


-- init : Session -> Api.Check ->(Model, Cmd Msg)
type alias MyData =
    { exercise : Int
    , share : Int
    , user : UserData }

type alias UserData =
    { id : Int
    , nickname : Maybe String
    , username : String}

init session mobile
    = (
        { session = session
        , check = mobile
        , currentPage = ""
        , nickname = ""
        , mydata = 
            { exercise = 0
            , share = 0
            , user = 
                { id = 0
                , nickname = Nothing
                , username = ""}
             }}
        , Api.get MyInfoData Endpoint.myInfo (Session.cred session) (Decoder.dataWRap My.DataWrap MyData UserData)
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

type Msg 
    = BackBtn
    | MyInfoData (Result Http.Error My.DataWrap)
    | ChangePage String
    | ChangeNick String
    | SuccessNickname (Result Http.Error Decoder.Success)
    | ChangeGo
    | KeyDown Int
    | AccountDelete
    | DeleteSuccess (Result Http.Error Decoder.Success)

toSession : Model -> Session
toSession model =
    model.session

toCheck : Model -> Bool
toCheck model =
    model.check

onKeyDown:(Int -> msg) -> Attribute msg
onKeyDown tagger = 
    on "keydown" (Decode.map tagger keyCode)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteSuccess (Ok ok) ->
            (model,Route.pushUrl (Session.navKey model.session) Route.Logout)
        DeleteSuccess (Err err) ->
            (model,Cmd.none)
        AccountDelete ->
            let
                body = Encode.object [("is_leave", Encode.bool True)]
                    |> Http.jsonBody
            in
            
            (model, Api.post Endpoint.accountDelete (Session.cred model.session) DeleteSuccess body  Decoder.resultD )
        KeyDown key ->
            if key == 13 then
                update ChangeGo model
            else
                (model, Cmd.none)
        SuccessNickname (Ok ok) ->
            ({model | currentPage = ""}, Api.get MyInfoData Endpoint.myInfo (Session.cred model.session) (Decoder.dataWRap My.DataWrap MyData UserData))
        SuccessNickname (Err err) ->
            (model, Cmd.none)
        ChangeGo ->
            let
                list = 
                    "nickname="
                    ++ model.nickname
                        |> Http.stringBody "application/x-www-form-urlencoded"
            in
            (model, Cmd.batch 
            [ Api.post Endpoint.changeNick (Session.cred model.session)  SuccessNickname list Decoder.resultD])
        ChangeNick str ->
            ({model | nickname = str},Cmd.none)
        ChangePage str ->
            ({model | currentPage = str}, Cmd.none)
        MyInfoData (Ok ok) ->
            ({model | mydata = ok.data}, Cmd.none)
        MyInfoData (Err err) ->
            let _ = Debug.log "err" err
                
            in
            
            (model, Cmd.none)
        BackBtn ->
            ( model, Route.pushUrl (Session.navKey model.session) Route.MyPage )

view : Model -> {title : String , content : Html Msg}
view model =
    {
    
    title = "YourFitExer"
    , content = 
        if model.currentPage == "nick" then
                div [] [
                    appHeaderConfirmDetailR "마이페이지" "myPageHeader" (ChangePage "")  ChangeGo  "확인"
                    , nicknameContents model 
                ]
        else if model.currentPage == "account" then
                div [] [
                    appHeaderback "마이페이지" "myPageHeader" BackBtn
                    , accountContents model
                ]
        else
               div [] [
                    appHeaderback "마이페이지" "myPageHeader" BackBtn
                    , contents model
                ]             
    
    }
justData cases = 
    case cases of
        Just a ->
            a
    
        Nothing ->
            " - "
nicknameContents model =    
    div [] [
        text "닉네임 변경"
        , input [onKeyDown KeyDown, type_ "text", onInput ChangeNick ] []
    ]

accountContents model = 
    div [] [
        text "계정 탈퇴 시 모든 정보가 삭제 됩니다. "
        , div [class "button", onClick AccountDelete] [text "계정 탈퇴"]
    ]
contents model = 
        div [ class "container yf_container" ]
            [ div [ class "m_yf_mypage_setting" ]
                [ div [ class "m_mypage_profilebox2" ]
                    [ img [ src "../image/profile.png" ]
                        []
                    , p [class"m_account_id"]
                        [ text (justData model.mydata.user.nickname) ]
                    ]
                , div [ class "m_mypage_loginbox_info" ]
                    [ p [ class "m_logintext" ]
                        [ text "로그인ID" ]
                    , p [ class "login_id" ]
                        [ text model.mydata.user.username ]
                    ]
                , p [ class "m_myPage_yf_terms" ]
                    [ a [ ]
                        [ text "개인정보 보호 및 약관확인" ]
                    ]
                , div [ class "m_mypage_setting_settingbox" ]
                    [ div [ class "button is-large is-fullwidth m_settingmenu", onClick (ChangePage "nick") ]
                        [ text "닉네임을 등록해주세요" ]
                    -- , a [ class "button is-large is-fullwidth m_settingmenu" ]
                    --     [ text "신체기록 관리" ]
                    , div [ class "button is-large is-fullwidth m_settingmenu", onClick (ChangePage "account") ]
                        [ text "계정관리" ]
                    ]
                , p [ class "m_yf_version" ]
                    [ text "버전정보 v1.0" ]
                ]
            ]