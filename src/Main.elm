port module Main exposing (..)
import Browser
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing ( .. )
import Html.Attributes exposing ( .. )
import Url exposing (Url)
import Html.Attributes as Attr
import Url.Parser as UrlParser
import Browser.Navigation as Nav
import Route exposing (Route)
import Session exposing (Session)
import Page exposing (Page)
import Page.Blank as Blank
import Page.Home as Home
import Page.MakeExer as MakeExer
import Page.MyPage as MyPage
import Page.Together as Together
import Page.YourFitExer as YourfitExer
import Page.Detail.YourFitDetail as YourfitDetail
import Page.YourFitExerList as YourFitList
import Page.Filter.Filter as Filter
import Page.Filter.FilterStep1 as FilterS1
import Page.Filter.FilterStep2 as FilterS2
import Page.Detail.TogetherWrite as TogetherW
import Page.MyPageMenu.Faq as Faq
import Page.MyPageMenu.Info as Info
import Page.MyPageMenu.MealRecord as MealR
import Page.MyPageMenu.MyCalendar as MyC
import Page.MyPageMenu.MyPost as MyPost
import Page.MyPageMenu.MyScrap as MyScrap
import Page.MyPageMenu.MyStatistical as MyS
import Json.Encode as Encode exposing (Value) 
import Json.Decode as Decode
import Page.Detail.MakeExerDetail as MakeDetail
import Page.Detail.MyAccount as MyAccount
import Page.MyPageMenu.MealRecordM as MealRM
import Page.Detail.InfoDetail as InfoD
import Page.Detail.FaqDetail as FaqD
import Page.Detail.FaqWrite as FaqW
import Page.Login as Login
import Page.Signup as Signup
import Api exposing(..)
import Page.SignupComplete as SC
import Page.Edit.MakeExerEdit as MakeEdit
import Page.EmptyPage as Empty
import Page.Edit.EditFilter as EditFilter
import Page.Detail.MyScrapDetail as ScrapD
import Page.Detail.MyPostDetail as PostD
import Page.Detail.MakeExerSearch as MSearch
import Page.Edit.MakeExerEditStepLast as MakeEditLast
import Page.Private as Private
import Page.SetPwd as SetPwd
import Page.ForgotPwd as FPwd
import Page.MyPageMenu.Contact as C
import Page.Detail.ContactDetail as CD
import TextApi as TA
import Page.MyPageMenu.PaperWeightList as MJList
-- type alias Check = 
--     String

type alias MainModel = 
    { mypageMenu : String }

type Model 
     = Redirect Session Bool
     | Home Home.Model
     | MakeExerModel MakeExer.Model
     | MyPageModel MyPage.Model
     | TogetherModel Together.Model
     | YourfitExerModel YourfitExer.Model
     | YourfitDetailModel YourfitDetail.Model
     | YourFitListModel YourFitList.Model
     | FilterModel Filter.Model
     | FilterS1Model FilterS1.Model
     | FilterS2Model FilterS2.Model
     | TogetherWModel TogetherW.Model
     | FaqModel Faq.Model
     | InfoModel Info.Model
     | MealRModel MealR.Model
     | MyCModel MyC.Model
     | MyPostModel MyPost.Model
     | MyScrapModel MyScrap.Model
     | MySModel MyS.Model
     | MakeDetailModel MakeDetail.Model
     | MyAccountModel MyAccount.Model
     | MealRMModel MealRM.Model
     | InfoDModel InfoD.Model
     | FaqDModel FaqD.Model
     | FaqWModel FaqW.Model
     | LoginModel Login.Model
     | SignupModel Signup.Model
     | SCModel SC.Model
     | MakeEditModel MakeEdit.Model
     | EmptyModel Empty.Model
     | ScrapDModel ScrapD.Model
     | PostDModel PostD.Model
     | EditFilterModel EditFilter.Model
     | MSearchModel MSearch.Model
     | MakeEditLastModel MakeEditLast.Model
     | PrivateModel Private.Model
     | SetPwdModel SetPwd.Model
     | FPwdModel FPwd.Model
     | TAModel TA.Model
     | CModel C.Model
     | CDModel CD.Model
     | MJListModel MJList.Model
    --  | PageModel Page.Model
    --  | CheckDevice Check
    



type Msg
    = Ignored
    | ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | HomeMsg Home.Msg
    | MakeExerMsg MakeExer.Msg
    | MyPageMsg MyPage.Msg
    | TogetherMsg Together.Msg
    | YourfitExerMsg YourfitExer.Msg
    | YourfitDetailMsg YourfitDetail.Msg
    | YourFitListMsg YourFitList.Msg
    | FilterMsg Filter.Msg
    | FilterS1Msg FilterS1.Msg
    | FilterS2Msg FilterS2.Msg
    | TogetherWMsg TogetherW.Msg
    | FaqMsg Faq.Msg
    | InfoMsg Info.Msg
    | MealRMsg MealR.Msg
    | MyCMsg MyC.Msg
    | MyPostMsg MyPost.Msg
    | MyScrapMsg MyScrap.Msg
    | MySMsg MyS.Msg
    | MakeDetailMsg MakeDetail.Msg 
    | MyAccountMsg MyAccount.Msg
    | MealRMMsg MealRM.Msg
    | InfoDMsg InfoD.Msg
    | FaqDMsg FaqD.Msg
    | FaqWMsg FaqW.Msg
    | LoginMsg Login.Msg
    | SignupMsg Signup.Msg
    | SCMsg SC.Msg
    | MakeEditMsg MakeEdit.Msg
    | EmptyMsg Empty.Msg
    | EditFilterMsg EditFilter.Msg
    | ScrapDMsg ScrapD.Msg
    | PostDMsg PostD.Msg
    | MSearchMsg MSearch.Msg
    | MakeEditLastMsg MakeEditLast.Msg
    | PrivateMsg Private.Msg
    | SetPwdMsg SetPwd.Msg
    | FPwdMsg FPwd.Msg
    | TAMsg TA.Msg
    | CMsg C.Msg
    | CDMsg CD.Msg
    | MJListMsg MJList.Msg

subscriptions model = 
    case model of
        Redirect session _ ->
            Sub.none
        Home item ->
            Sub.map HomeMsg (Home.subscriptions item)
            -- Sub.none
        MakeExerModel item ->
            Sub.map MakeExerMsg (MakeExer.subscriptions item)
        MyPageModel item->
            Sub.map MyPageMsg (MyPage.subscriptions item)
        TogetherModel item ->
            Sub.map TogetherMsg (Together.subscriptions item)
        YourfitExerModel item ->
            -- Sub.none
            Sub.map YourfitExerMsg (YourfitExer.subscriptions item)
        YourfitDetailModel item ->
            Sub.map YourfitDetailMsg (YourfitDetail.subscriptions item)
        YourFitListModel item ->
            Sub.map YourFitListMsg (YourFitList.subscriptions item)
        FilterModel item ->
            Sub.map FilterMsg (Filter.subscriptions item)
        FilterS1Model item ->
            Sub.map FilterS1Msg (FilterS1.subscriptions item)
        FilterS2Model item ->
            Sub.map FilterS2Msg (FilterS2.subscriptions item)
        TogetherWModel item ->
            Sub.map TogetherWMsg (TogetherW.subscriptions item)
        FaqModel item ->
            Sub.map FaqMsg (Faq.subscriptions item)
        InfoModel item ->
            Sub.map InfoMsg (Info.subscriptions item)
        MealRModel item ->
            Sub.map MealRMsg (MealR.subscriptions item)
        MyCModel item ->
            Sub.map MyCMsg (MyC.subscriptions item)
        MyPostModel item ->
            Sub.map MyPostMsg (MyPost.subscriptions item)
        MyScrapModel item ->
            Sub.map MyScrapMsg (MyScrap.subscriptions item)
        MySModel item ->
            Sub.map MySMsg (MyS.subscriptions item)
        MakeDetailModel item ->
            Sub.map MakeDetailMsg (MakeDetail.subscriptions item)
        MyAccountModel item ->
            Sub.map MyAccountMsg (MyAccount.subscriptions item)
        MealRMModel item ->
            Sub.map MealRMMsg (MealRM.subscriptions item)
        InfoDModel item ->
            Sub.map InfoDMsg(InfoD.subscriptions item)
        FaqDModel item ->
            Sub.map FaqDMsg (FaqD.subscriptions item)
        FaqWModel item ->
            Sub.map FaqWMsg (FaqW.subscriptions item)
        LoginModel item ->
            Sub.map LoginMsg (Login.subscriptions item)
        SignupModel item ->
            Sub.map SignupMsg (Signup.subscriptions item)
        SCModel item ->
            Sub.none
        MakeEditModel item ->
            Sub.map MakeEditMsg (MakeEdit.subscriptions item)
        EmptyModel item ->
            Sub.none
        EditFilterModel item ->
            Sub.map EditFilterMsg (EditFilter.subscriptions item)
        ScrapDModel item ->
            Sub.map ScrapDMsg (ScrapD.subscriptions item)
        PostDModel item ->
            Sub.map PostDMsg (PostD.subscriptions item)
        MSearchModel item ->
            Sub.map MSearchMsg (MSearch.subscriptions item)
        MakeEditLastModel item ->
            Sub.map MakeEditLastMsg (MakeEditLast.subscriptions item)
        PrivateModel item ->
            Sub.none
        SetPwdModel item ->
            Sub.none
        FPwdModel item ->
            Sub.none
        TAModel item ->
            Sub.none
        CModel item ->
            Sub.map CMsg (C.subscriptions item)
        CDModel item ->
            Sub.map CDMsg (CD.subscriptions item)
        MJListModel item ->
            Sub.map MJListMsg (MJList.subscriptions item)

init : Maybe Cred -> Bool -> Url -> Key -> ( Model, Cmd Msg )
init maybeViewer check url navKey =
        changeRouteTo (Route.fromUrl url) (Redirect (Session.fromViewer navKey maybeViewer) check)

-- changeRouteTo : Maybe Route ->  Model  -> (Model , Cmd Msg)
changeRouteTo maybeRoute model =
    let
        session = toSession model
        check = toCheck model

    in
    case maybeRoute of
        Nothing -> 
            Home.init session check
                |> updateWith Home HomeMsg model 
        Just Route.Other ->
            (Redirect session check  , Cmd.none)
        Just Route.Home -> 
            Home.init session  check
                |> updateWith Home HomeMsg model
        Just Route.YourFitExer ->
            YourfitExer.init session check    
                |> updateWith YourfitExerModel YourfitExerMsg model
        Just Route.MakeExer ->
            MakeExer.init session check   
                |> updateWith MakeExerModel MakeExerMsg model
        Just Route.Together ->
            Together.init session check
                |> updateWith TogetherModel TogetherMsg model
        Just Route.MyPage ->
            MyPage.init session check
                |> updateWith MyPageModel MyPageMsg model
        Just Route.YourfitDetail ->
            YourfitDetail.init session check
                |> updateWith YourfitDetailModel YourfitDetailMsg model
        Just Route.YourFitList ->
            YourFitList.init session check
                |> updateWith YourFitListModel YourFitListMsg model   
        Just Route.Filter ->
            Filter.init session check
                |> updateWith FilterModel FilterMsg model 
        Just Route.FilterS1 ->
            FilterS1.init session check
                |> updateWith FilterS1Model FilterS1Msg model
        Just Route.FilterS2 ->
            FilterS2.init session check
                |> updateWith FilterS2Model FilterS2Msg model
        Just Route.TogetherW ->
            TogetherW.init session check
                |> updateWith TogetherWModel TogetherWMsg model
        Just Route.Faq ->
            Faq.init session check
                |> updateWith FaqModel FaqMsg model
        Just Route.Info ->
            Info.init session check
                |> updateWith InfoModel InfoMsg model
        Just Route.MealR ->
            MealR.init session check
                |> updateWith MealRModel MealRMsg model
        Just Route.MyC ->
            MyC.init session check
                |> updateWith MyCModel MyCMsg model
        Just Route.MyPost ->
            MyPost.init session check
                |> updateWith  MyPostModel MyPostMsg model
        Just Route.MyScrap ->
            MyScrap.init session check
                |> updateWith  MyScrapModel MyScrapMsg model
        Just Route.MyS ->
            MyS.init session check
                |> updateWith MySModel MySMsg model
        Just Route.MakeDetail ->
            MakeDetail.init session check
                |> updateWith MakeDetailModel MakeDetailMsg model
        Just Route.MyAccount ->
            MyAccount.init session check
                |> updateWith MyAccountModel MyAccountMsg model
        Just Route.MealRM ->
            MealRM.init session check
                |> updateWith MealRMModel MealRMMsg model
        Just Route.InfoD ->
            InfoD.init session check
                |> updateWith InfoDModel InfoDMsg model
        Just Route.FaqD ->
            FaqD.init session check
                |> updateWith FaqDModel FaqDMsg model
        Just Route.FaqW ->
            FaqW.init session check
                |> updateWith FaqWModel FaqWMsg model
        Just Route.Login ->
            Login.init session check
                |> updateWith LoginModel LoginMsg model
        Just Route.Signup ->
            Signup.init session check
                |> updateWith SignupModel SignupMsg model
        Just Route.SC ->
            SC.init session check
                |> updateWith SCModel SCMsg model
        Just Route.MakeEdit ->
            MakeEdit.init session check
                |> updateWith MakeEditModel MakeEditMsg model
        Just Route.Empty ->
            Empty.init session check
                |> updateWith EmptyModel EmptyMsg model
        Just Route.Logout ->
            (model, Cmd.batch[Api.logoutpop ()])
            
        Just Route.EditFilter ->
            EditFilter.init session check
                |> updateWith EditFilterModel EditFilterMsg model
        Just Route.ScrapD ->
            ScrapD.init session check
                |> updateWith ScrapDModel ScrapDMsg model
        Just Route.PostD ->
            PostD.init session check
                |> updateWith PostDModel PostDMsg model

        Just Route.MSearch ->
            MSearch.init session check
                |> updateWith MSearchModel MSearchMsg model
        Just Route.MakeEditLast ->
            MakeEditLast.init session check
                |> updateWith MakeEditLastModel MakeEditLastMsg model
        Just Route.Private ->
            Private.init session check
                |> updateWith PrivateModel PrivateMsg model
        Just Route.SetPwd ->
            SetPwd.init session check
                |> updateWith SetPwdModel SetPwdMsg model
        Just Route.FPwd ->
            FPwd.init session check
                |> updateWith FPwdModel FPwdMsg model
        Just Route.LogoutConfirm ->
            (model, Cmd.batch[Api.logout,Api.logoutpop (), Route.load "#/home"])
        Just Route.TA ->
            TA.init session check
                |> updateWith TAModel TAMsg model
        Just Route.MyPageBottomMenu ->
            (model, Api.mypageMenu (Encode.bool True) )
        Just Route.C ->
            C.init session check
                |> updateWith CModel CMsg model
        Just Route.CD ->
            CD.init session check
                |> updateWith CDModel CDMsg model
        Just Route.MJList ->
            MJList.init session check
                |> updateWith MJListModel MJListMsg model
            


toCheck page =  
    case page of
        Redirect session check ->
            check
        Home user ->
            Home.toCheck user
        MakeExerModel item ->
            MakeExer.toCheck item
        MyPageModel item ->
            MyPage.toCheck item
        TogetherModel item ->
            Together.toCheck item
        YourfitExerModel item ->
            YourfitExer.toCheck item
        YourfitDetailModel item ->
            YourfitDetail.toCheck item
        YourFitListModel item ->
            YourFitList.toCheck item
        FilterModel item ->
            Filter.toCheck item
        FilterS1Model item ->
            FilterS1.toCheck item
        FilterS2Model item ->
            FilterS2.toCheck item
        TogetherWModel item ->
            TogetherW.toCheck item
        FaqModel item ->
            Faq.toCheck item
        InfoModel item ->
            Info.toCheck item
        MealRModel item ->
            MealR.toCheck item
        MyCModel item ->
            MyC.toCheck item
        MyPostModel item ->
            MyPost.toCheck item
        MyScrapModel item ->
            MyScrap.toCheck item
        MySModel item ->
            MyS.toCheck item
        MakeDetailModel item ->
            MakeDetail.toCheck item
        MyAccountModel item ->
            MyAccount.toCheck item
        MealRMModel item ->
            MealRM.toCheck item
        InfoDModel item ->
            InfoD.toCheck item
        FaqDModel item ->
            FaqD.toCheck item
        FaqWModel item ->
            FaqW.toCheck item
        LoginModel item ->
            Login.toCheck item
        SignupModel item ->
            Signup.toCheck item
        SCModel item ->
            SC.toCheck item
        MakeEditModel item ->
            MakeEdit.toCheck item
        EmptyModel item ->
            Empty.toCheck item
        EditFilterModel item ->
            EditFilter.toCheck item
        ScrapDModel item ->
            ScrapD.toCheck item
        PostDModel item ->  
            PostD.toCheck item
        MSearchModel item ->
            MSearch.toCheck item
        MakeEditLastModel item ->
            MakeEditLast.toCheck item
        PrivateModel item ->
            Private.toCheck item
        SetPwdModel item ->
            SetPwd.toCheck item
        FPwdModel item ->
            FPwd.toCheck item
        TAModel item ->
            TA.toCheck item
        CModel item ->
            C.toCheck item
        CDModel item ->
            CD.toCheck item
        MJListModel item ->
            MJList.toCheck item


toSession : Model -> Session
toSession page =
    case page of
        Redirect session _->
            session
        Home user ->
            Home.toSession user
        MakeExerModel item ->
            MakeExer.toSession item
        MyPageModel item ->
            MyPage.toSession item
        TogetherModel item ->
            Together.toSession item
        YourfitExerModel item ->
            YourfitExer.toSession item
        YourfitDetailModel item ->
            YourfitDetail.toSession item
        YourFitListModel item ->
            YourFitList.toSession item
        FilterModel item ->
            Filter.toSession item
        FilterS1Model item ->
            FilterS1.toSession item
        FilterS2Model item ->
            FilterS2.toSession item
        TogetherWModel item ->
            TogetherW.toSession item
        FaqModel item ->
            Faq.toSession item
        InfoModel item ->
            Info.toSession item
        MealRModel item ->
            MealR.toSession item
        MyCModel item ->
            MyC.toSession item
        MyPostModel item ->
            MyPost.toSession item
        MyScrapModel item ->
            MyScrap.toSession item
        MySModel item ->
            MyS.toSession item
        MakeDetailModel item ->
            MakeDetail.toSession item
        MyAccountModel item ->
            MyAccount.toSession item
        MealRMModel item ->
            MealRM.toSession item
        InfoDModel item ->
            InfoD.toSession item
        FaqDModel item ->
            FaqD.toSession item
        FaqWModel item ->
            FaqW.toSession item
        LoginModel item ->
            Login.toSession item
        SignupModel item ->
            Signup.toSession item
        SCModel item ->
            SC.toSession item
        MakeEditModel item ->
            MakeEdit.toSession item
        EmptyModel item ->
            Empty.toSession item
        EditFilterModel item ->
            EditFilter.toSession item
        ScrapDModel item ->
            ScrapD.toSession item
        PostDModel item ->
            PostD.toSession item
        MSearchModel item ->
            MSearch.toSession item
        MakeEditLastModel item ->
            MakeEditLast.toSession item
        PrivateModel item ->
            Private.toSession item
        SetPwdModel item ->
            SetPwd.toSession item
        FPwdModel item ->
            FPwd.toSession item
        TAModel item ->
            TA.toSession item
        CModel item ->
            C.toSession item
        CDModel item ->
            CD.toSession item
        MJListModel item ->
            MJList.toSession item
        

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )
        -- (UpdateStr subMsg , CheckDevice cdmodel) ->
        --     (model, Cmd.none)
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            ( model, Cmd.none )

                        Just _ ->
                            ( model
                            , Cmd.batch[Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                            , Api.removeJw () ]
                            
                            )
                            

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model
        
        (HomeMsg subMsg , Home homeModel) ->
            Home.update subMsg homeModel
                |> updateWith Home HomeMsg model

        (MakeExerMsg subMsg , MakeExerModel makemodel) ->
            MakeExer.update subMsg makemodel
                |> updateWith MakeExerModel MakeExerMsg model

        (MyPageMsg subMsg , MyPageModel mypagemodel) ->
            MyPage.update subMsg mypagemodel
                |> updateWith MyPageModel MyPageMsg model

        (TogetherMsg subMsg , TogetherModel tmodel) ->
            Together.update subMsg tmodel
                |> updateWith TogetherModel TogetherMsg model

        (YourfitExerMsg subMsg , YourfitExerModel xmodel) ->
            YourfitExer.update subMsg xmodel
                |> updateWith YourfitExerModel YourfitExerMsg model
        (YourfitDetailMsg subMsg , YourfitDetailModel ydmodel) ->
            YourfitDetail.update subMsg ydmodel
                |> updateWith YourfitDetailModel YourfitDetailMsg model
        (YourFitListMsg subMsg ,YourFitListModel ylmodel) ->
            YourFitList.update subMsg ylmodel
                |> updateWith YourFitListModel YourFitListMsg model
        (FilterMsg subMsg, FilterModel fmodel) ->
            Filter.update subMsg fmodel
                |> updateWith FilterModel FilterMsg model
        (FilterS1Msg subMsg, FilterS1Model f1model) ->
            FilterS1.update subMsg f1model
                |> updateWith FilterS1Model FilterS1Msg model
        (FilterS2Msg subMsg, FilterS2Model f2model) ->
            FilterS2.update subMsg f2model
                |> updateWith FilterS2Model FilterS2Msg model
        (TogetherWMsg subMsg, TogetherWModel twmodel) ->
            TogetherW.update subMsg twmodel
                |> updateWith TogetherWModel TogetherWMsg model
        (FaqMsg subMsg, FaqModel fmodel) ->
            Faq.update subMsg fmodel
                |> updateWith FaqModel FaqMsg model
        (InfoMsg subMsg, InfoModel imodel) ->
            Info.update subMsg imodel
                |> updateWith InfoModel InfoMsg imodel
        (MealRMsg subMsg, MealRModel mmodel) ->
            MealR.update subMsg mmodel
                |> updateWith MealRModel MealRMsg model
        (MyCMsg subMsg, MyCModel cmodel) ->
            MyC.update subMsg cmodel
                |> updateWith MyCModel MyCMsg model
        (MyPostMsg subMsg , MyPostModel pmodel) ->
            MyPost.update subMsg pmodel
                |> updateWith MyPostModel MyPostMsg model
        (MyScrapMsg subMsg, MyScrapModel smodel) ->
            MyScrap.update subMsg smodel
                |> updateWith MyScrapModel MyScrapMsg model
        (MySMsg subMsg, MySModel smodel) ->
            MyS.update subMsg smodel
                |> updateWith MySModel MySMsg model
        (MakeDetailMsg subMsg, MakeDetailModel mmodel) ->
            MakeDetail.update subMsg mmodel
                |> updateWith MakeDetailModel MakeDetailMsg model
        (MyAccountMsg subMsg, MyAccountModel mmodel) ->
            MyAccount.update subMsg mmodel
                |> updateWith MyAccountModel MyAccountMsg model
        (MealRMMsg subMsg, MealRMModel mmodel) ->
            MealRM.update subMsg mmodel
                |> updateWith MealRMModel MealRMMsg model
        (InfoDMsg subMsg, InfoDModel imodel) ->
            InfoD.update subMsg imodel
                |> updateWith InfoDModel InfoDMsg model
        (FaqDMsg subMsg, FaqDModel fmodel) ->
            FaqD.update subMsg fmodel
                |> updateWith FaqDModel FaqDMsg model
        (FaqWMsg subMsg , FaqWModel fmodel) ->
            FaqW.update subMsg fmodel
                |> updateWith FaqWModel FaqWMsg model
        (LoginMsg subMsg , LoginModel lmodel) ->
            Login.update subMsg lmodel
                |> updateWith LoginModel LoginMsg lmodel
        (SignupMsg subMsg, SignupModel smodel) ->
            Signup.update subMsg smodel
                |>updateWith SignupModel SignupMsg smodel
        (MakeEditMsg subMsg, MakeEditModel mmodel) ->
            MakeEdit.update subMsg mmodel
                |> updateWith MakeEditModel MakeEditMsg mmodel
        (EmptyMsg subMsg, EmptyModel emodel) ->
            Empty.update subMsg emodel
                |> updateWith EmptyModel EmptyMsg emodel
        (EditFilterMsg subMsg, EditFilterModel emodel) ->
            EditFilter.update subMsg emodel
                |> updateWith EditFilterModel EditFilterMsg emodel
        (PostDMsg subMsg, PostDModel pmodel) ->
            PostD.update subMsg pmodel
                |> updateWith PostDModel PostDMsg pmodel
        (ScrapDMsg subMsg, ScrapDModel smodel) ->
            ScrapD.update subMsg smodel
                |> updateWith ScrapDModel ScrapDMsg smodel
        (MSearchMsg subMsg, MSearchModel mmodel) ->
            MSearch.update subMsg mmodel
                |> updateWith MSearchModel MSearchMsg mmodel
        (MakeEditLastMsg subMsg, MakeEditLastModel mmodel) ->
            MakeEditLast.update subMsg mmodel
                |> updateWith MakeEditLastModel MakeEditLastMsg mmodel
        (PrivateMsg subMsg, PrivateModel pmodel) ->
            Private.update subMsg pmodel
                |> updateWith PrivateModel PrivateMsg pmodel
        (SetPwdMsg subMsg, SetPwdModel smodel) ->
            SetPwd.update subMsg smodel
                |> updateWith SetPwdModel SetPwdMsg smodel
        (FPwdMsg subMsg , FPwdModel fmodel) ->
            FPwd.update subMsg fmodel
                |> updateWith FPwdModel FPwdMsg fmodel
        (TAMsg subMsg, TAModel tmodel) ->
            TA.update subMsg tmodel
                |> updateWith TAModel TAMsg tmodel
        (CMsg subMsg, CModel cmodel) ->
            C.update subMsg cmodel
                |> updateWith CModel CMsg cmodel
        (CDMsg subMsg, CDModel cmodel) ->
            CD.update subMsg cmodel
                |> updateWith CDModel CDMsg cmodel
        (MJListMsg subMsg, MJListModel mmodel) ->
            MJList.update subMsg mmodel
                |> updateWith MJListModel MJListMsg mmodel
        ( _, _ ) ->
            ( model, Cmd.none )  
        
            
updateWith toModel toMsg model (subModel, subCmd) =
    (toModel subModel
    , Cmd.map toMsg subCmd)
            
            
         
                    
view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (Session.cred (toSession model)) (toCheck model) page config
            in
            {
            title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ _->
            viewPage Page.Other (\_ -> Ignored) Blank.view
        -- CheckDevice item ->
        --     viewPage Page.Other (\_ -> Ignored) Blank.view
        Home home ->
            viewPage Page.Home HomeMsg (Home.view home)
        
        MakeExerModel item ->
            viewPage Page.MakeExer MakeExerMsg (MakeExer.view item)

        MyPageModel item ->
            viewPage Page.MyPage MyPageMsg (MyPage.view item)

        TogetherModel item ->
            viewPage Page.Together TogetherMsg (Together.view item)

        YourfitExerModel item ->
            viewPage Page.YourfitExer YourfitExerMsg (YourfitExer.view item)
        
        YourfitDetailModel item ->
            viewPage Page.YourfitDetail YourfitDetailMsg (YourfitDetail.view item)

        YourFitListModel item ->
            viewPage Page.YourFitList YourFitListMsg (YourFitList.view item)

        FilterModel item ->
            viewPage Page.Filter FilterMsg(Filter.view item)

        FilterS1Model item ->
            viewPage Page.FilterS1 FilterS1Msg(FilterS1.view item)

        FilterS2Model item ->
            viewPage Page.FilterS2 FilterS2Msg(FilterS2.view item)
        
        TogetherWModel item ->
            viewPage Page.TogetherW TogetherWMsg (TogetherW.view item)
        FaqModel item ->
            viewPage Page.Faq FaqMsg (Faq.view item)
        InfoModel item ->
            viewPage Page.Info InfoMsg (Info.view item)
        MealRModel item ->
            viewPage Page.MealR MealRMsg (MealR.view item)
        MyCModel item ->
            viewPage Page.MyC MyCMsg (MyC.view item)
        MyPostModel item ->
            viewPage Page.MyPost MyPostMsg (MyPost.view item)
        MyScrapModel item ->
            viewPage Page.MyScrap MyScrapMsg (MyScrap.view item)
        MySModel item ->
            viewPage Page.MyS MySMsg (MyS.view item)
        MakeDetailModel item ->
            viewPage Page.MakeDetail MakeDetailMsg (MakeDetail.view item)
        MyAccountModel item ->
            viewPage Page.MyAccount MyAccountMsg (MyAccount.view item)
        MealRMModel item ->
            viewPage Page.MealRM MealRMMsg (MealRM.view item)
        InfoDModel item ->
            viewPage Page.InfoD InfoDMsg (InfoD.view item)
        FaqDModel item ->
            viewPage Page.FaqD FaqDMsg (FaqD.view item)
        FaqWModel item ->
            viewPage Page.FaqW FaqWMsg (FaqW.view item)
        LoginModel item ->   
            viewPage Page.Login LoginMsg (Login.view item)
        SignupModel item ->
            viewPage Page.Signup SignupMsg (Signup.view item)
        SCModel item ->
            viewPage Page.SC SCMsg (SC.view item)
        MakeEditModel item ->
            viewPage Page.MakeEdit MakeEditMsg(MakeEdit.view item)
        EmptyModel item ->
            viewPage Page.Empty EmptyMsg (Empty.view item)
        EditFilterModel item ->
            viewPage Page.EditFilter EditFilterMsg(EditFilter.view item)
        ScrapDModel item ->
            viewPage Page.ScrapD ScrapDMsg (ScrapD.view item)
        PostDModel item ->
            viewPage Page.PostD PostDMsg (PostD.view item)
        MSearchModel item ->
            viewPage Page.MSearch MSearchMsg (MSearch.view item)
        MakeEditLastModel item ->
            viewPage Page.MakeEditLast MakeEditLastMsg (MakeEditLast.view item)
        PrivateModel item ->
            viewPage Page.Private PrivateMsg (Private.view item)
        SetPwdModel item ->
            viewPage Page.SetPwd SetPwdMsg (SetPwd.view item)
        FPwdModel item ->
            viewPage Page.FPwd FPwdMsg (FPwd.view item)
        TAModel item ->
            viewPage Page.TA TAMsg (TA.view item)
        CModel item ->
            viewPage Page.C CMsg (C.view item)
        CDModel item ->
            viewPage Page.CD CDMsg (CD.view item)
        MJListModel item ->
            viewPage Page.MJList MJListMsg (MJList.view item)


main : Program Value Model Msg
main =
    Api.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }