port module Main exposing (main)

import Array
import Browser
import Browser.Dom
import Browser.Navigation
import CommonUi
import Css
import Data
import Data.Key
import Data.LogInState
import Data.TimeZoneAndName
import Data.UrlData
import Html.Styled
import Json.Decode
import Json.Encode
import Message
import Page.CreateIdea
import Page.CreateProject
import Page.Home
import Page.Idea
import Page.Project
import Page.Suggestion
import Page.User
import Task
import Time
import Ui
import Url


{-| すべての状態を管理する
-}



{- Cmd (Elm → JavaScript) -}


port setTextAreaValue : { text : String, id : String } -> Cmd msg


port focusElement : String -> Cmd msg


port preventDefaultBeforeKeyEvent : () -> Cmd msg


port elementScrollIntoView : String -> Cmd msg


port consoleLog : String -> Cmd msg


port requestLogInUrl : Json.Encode.Value -> Cmd msg


port getUserByAccessToken : Json.Encode.Value -> Cmd msg


port getImageBlobUrl : Json.Encode.Value -> Cmd msg


port createProject : Json.Encode.Value -> Cmd msg


port createIdea : Json.Encode.Value -> Cmd msg


port addComment : Json.Encode.Value -> Cmd msg


port addSuggestion : Json.Encode.Value -> Cmd msg


port toValidProjectName : String -> Cmd msg


port toValidIdeaName : String -> Cmd msg


port toValidTypePartName : String -> Cmd msg


port getUser : Json.Decode.Value -> Cmd msg


port getUserNoCache : Json.Decode.Value -> Cmd msg


port getAllProjectIdList : () -> Cmd msg


port getProject : Json.Decode.Value -> Cmd msg


port getProjectNoCache : Json.Decode.Value -> Cmd msg


port getIdeaAndIdListByProjectId : Json.Decode.Value -> Cmd msg


port getIdea : Json.Decode.Value -> Cmd msg


port getIdeaNoCache : Json.Decode.Value -> Cmd msg


port getSuggestion : Json.Decode.Value -> Cmd msg


port getSuggestionNoCache : Json.Decode.Value -> Cmd msg



{- Sub (JavaScript → Elm) -}


port responseUserByAccessToken : (Json.Decode.Value -> msg) -> Sub msg


port keyPressed : (Json.Decode.Value -> msg) -> Sub msg


port keyPrevented : (() -> msg) -> Sub msg


port windowResize : ({ width : Int, height : Int } -> msg) -> Sub msg


port changeNetworkConnection : (Bool -> msg) -> Sub msg


port subPointerUp : (() -> msg) -> Sub msg


port getImageBlobResponse : ({ blobUrl : String, imageToken : String } -> msg) -> Sub msg


port toValidProjectNameResponse : ({ input : String, result : Maybe String } -> msg) -> Sub msg


port toValidIdeaNameResponse : ({ input : String, result : Maybe String } -> msg) -> Sub msg


port toValidTypePartNameResponse : ({ input : String, result : Maybe String } -> msg) -> Sub msg


port createProjectResponse : (Json.Decode.Value -> msg) -> Sub msg


port responseCreateIdea : (Json.Decode.Value -> msg) -> Sub msg


port responseAddComment : (Json.Decode.Value -> msg) -> Sub msg


port responseAddSuggestion : (Json.Decode.Value -> msg) -> Sub msg


port responseAllProjectId : (Json.Decode.Value -> msg) -> Sub msg


port responseProject : (Json.Decode.Value -> msg) -> Sub msg


port responseUser : (Json.Decode.Value -> msg) -> Sub msg


port responseIdeaSnapshotAndIdListByProjectId : (Json.Decode.Value -> msg) -> Sub msg


port responseIdea : (Json.Decode.Value -> msg) -> Sub msg


port responseSuggestion : (Json.Decode.Value -> msg) -> Sub msg


{-| 全体の入力を表すメッセージ
-}
type Msg
    = KeyPrevented -- キーボードの入力のデフォルト動作を取り消した後
    | PointerUp -- マウスのボタンを離した/タブの表示状態が変わった
    | ToResizeGutterMode GutterType -- リサイズモードに移行
    | WindowResize { width : Int, height : Int } -- ウィンドウサイズを変更
    | LogOutRequest -- ログアウトを要求する
    | ChangeNetworkConnection Bool -- 接続状況が変わった
    | UpdateTime Time.Posix -- 時間が過ぎた
    | PageMsg PageMessage -- ページ固有のメッセージ
    | RequestLogInUrl Data.OpenIdConnectProvider
    | ResponseUserDataFromAccessToken (Maybe Data.UserSnapshotAndId)
    | ResponseImageBlob { blobUrl : String, imageToken : Data.ImageToken }
    | ResponseUser Data.UserResponse
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | ResponseTimeZone Data.TimeZoneAndName.TimeZoneAndName
    | CreateProjectResponse (Maybe Data.ProjectSnapshotAndId)
    | CreateIdeaResponse (Maybe Data.IdeaSnapshotAndId)
    | NotificationDeleteAt Int
    | KeyPressed Data.Key.Key
    | CommonMessage Message.CommonMessage
    | NoOperation


type PageMessage
    = PageMessageHome Page.Home.Message
    | PageMessageCreateProject Page.CreateProject.Message
    | PageMessageCreateIdea Page.CreateIdea.Message
    | PageMessageProject Page.Project.Message
    | PageMessageUser Page.User.Message
    | PageMessageIdea Page.Idea.Message
    | PageMessageSuggestion Page.Suggestion.Message


{-| 全体を表現する
-}
type Model
    = Model
        { subMode : SubMode
        , page : PageModel
        , messageQueue : List Msg
        , subModel : Message.SubModel
        , networkConnection : Bool
        , eventList : List Event
        , navigationKey : Browser.Navigation.Key
        }


type Event
    = LogInSuccess Data.UserSnapshotAndId
    | LogInFailure
    | OnLine
    | OffLine
    | CreatedProject Data.ProjectSnapshotAndId
    | CreateProjectFailed


type SubMode
    = SubModeNone
    | SubModeGutter GutterType


type GutterType
    = GutterTypeVertical
    | GutterTypeHorizontal


type PageModel
    = Home Page.Home.Model
    | CreateProject Page.CreateProject.Model
    | CreateIdea Page.CreateIdea.Model
    | Project Page.Project.Model
    | User Page.User.Model
    | Idea Page.Idea.Model
    | Suggestion Page.Suggestion.Model


type alias Flag =
    { windowSize :
        { width : Int
        , height : Int
        }
    , nowTime : Data.Time
    , accessTokenMaybe : Maybe String
    , networkConnection : Bool
    }


main : Platform.Program Flag Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : Flag -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flag url navigationKey =
    let
        urlData : Data.UrlData
        urlData =
            Data.UrlData.urlDataFromUrl url

        ( pageInitModel, pageCommand ) =
            pageInit urlData.location

        logInState =
            case flag.accessTokenMaybe of
                Just accessToken ->
                    Data.LogInState.VerifyingAccessToken (Data.AccessToken accessToken)

                Nothing ->
                    Data.LogInState.GuestUser
    in
    ( Model
        { subMode = SubModeNone
        , page = pageInitModel
        , messageQueue = []
        , subModel =
            Message.from
                { logInState = logInState
                , language = urlData.language
                , clientMode = urlData.clientMode
                , timeZoneAndNameMaybe = Nothing
                , nowTime = Data.TimeZoneAndName.timeToPosix flag.nowTime
                , windowSize = flag.windowSize
                }
        , networkConnection = flag.networkConnection
        , eventList =
            if flag.networkConnection then
                []

            else
                [ OffLine ]
        , navigationKey = navigationKey
        }
    , Cmd.batch
        [ case flag.accessTokenMaybe of
            Just accessToken ->
                getUserByAccessTokenTyped (Data.AccessToken accessToken)

            Nothing ->
                Cmd.none
        , commandToMainCommand logInState pageCommand
        , Browser.Navigation.replaceUrl navigationKey (Url.toString (Data.UrlData.urlDataToUrl urlData))
        , Task.perform
            ResponseTimeZone
            (Task.map2 Data.TimeZoneAndName.from Time.getZoneName Time.here)
        ]
    )


pageInit : Data.Location -> ( PageModel, Message.Command )
pageInit location =
    case location of
        Data.LocationCreateProject ->
            Page.CreateProject.init
                |> Tuple.mapFirst CreateProject

        Data.LocationCreateIdea projectId ->
            Page.CreateIdea.init projectId
                |> Tuple.mapFirst CreateIdea

        Data.LocationUser userId ->
            Page.User.init userId
                |> Tuple.mapFirst User

        Data.LocationProject projectId ->
            Page.Project.init projectId
                |> Tuple.mapFirst Project

        Data.LocationIdea ideaId ->
            Page.Idea.init ideaId
                |> Tuple.mapFirst Idea

        Data.LocationSuggestion suggestionId ->
            Page.Suggestion.init suggestionId
                |> Tuple.mapFirst Suggestion

        _ ->
            Page.Home.init
                |> Tuple.mapFirst Home



{- ============================================
                   Update
   ============================================
-}


{-| Definy全体のUpdate
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model rec) =
    case msg of
        KeyPrevented ->
            let
                ( listMsg, newModel ) =
                    Model rec |> shiftMsgListFromMsgQueue
            in
            newModel |> updateFromMsgList listMsg

        PointerUp ->
            pointerUp (Model rec)

        ToResizeGutterMode gutter ->
            ( toGutterMode gutter (Model rec)
            , Cmd.none
            )

        WindowResize { width, height } ->
            ( Model
                { rec | subModel = Message.setWindowSize { width = width, height = height } rec.subModel }
            , Cmd.none
            )

        LogOutRequest ->
            ( Model rec
            , Cmd.none
            )

        PageMsg pageMsg ->
            let
                ( newPageModel, command ) =
                    updatePage pageMsg (Model rec)
            in
            ( Model { rec | page = newPageModel }
            , commandToMainCommand (Message.getLogInState rec.subModel) command
            )

        ChangeNetworkConnection connection ->
            ( Model
                { rec
                    | eventList =
                        rec.eventList
                            ++ [ if connection then
                                    OnLine

                                 else
                                    OffLine
                               ]
                    , networkConnection = False
                }
            , Cmd.none
            )

        RequestLogInUrl openIdConnectProvider ->
            ( Model
                { rec
                    | subModel =
                        Message.setLogInState
                            (Data.LogInState.RequestLogInUrl openIdConnectProvider)
                            rec.subModel
                }
            , requestLogInUrlTyped
                { openIdConnectProvider = openIdConnectProvider
                , urlData =
                    { clientMode = Message.getClientMode rec.subModel
                    , location = pageModelToLocation rec.page
                    , language = Message.getLanguage rec.subModel
                    }
                }
            )

        ResponseUserDataFromAccessToken userSnapshotAndIdMaybe ->
            Model rec |> responseUserDataFromAccessToken userSnapshotAndIdMaybe

        ResponseImageBlob imageBlobAndFileHash ->
            ( Model
                { rec
                    | subModel =
                        Message.addImageBlobUrl
                            imageBlobAndFileHash.imageToken
                            imageBlobAndFileHash.blobUrl
                            rec.subModel
                }
            , Cmd.none
            )

        ResponseUser userResponse ->
            ( Model
                { rec
                    | subModel =
                        Message.addUserSnapshot
                            userResponse.snapshotMaybe
                            userResponse.id
                            rec.subModel
                }
            , commandToMainCommand
                (Message.getLogInState rec.subModel)
                (case userResponse.snapshotMaybe of
                    Just userSnapshot ->
                        Message.GetBlobUrl userSnapshot.imageHash

                    Nothing ->
                        Message.None
                )
            )

        OnUrlChange url ->
            let
                urlData =
                    Data.UrlData.urlDataFromUrl url

                ( newPage, command ) =
                    pageInit urlData.location
            in
            ( Model
                { rec
                    | page = newPage
                    , subModel =
                        Message.setLanguageAndClientMode
                            urlData.language
                            urlData.clientMode
                            rec.subModel
                }
            , commandToMainCommand (Message.getLogInState rec.subModel) command
            )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( Model rec
                    , Browser.Navigation.pushUrl
                        rec.navigationKey
                        (Url.toString
                            (Data.UrlData.urlDataToUrl
                                (Data.UrlData.urlDataFromUrl url)
                            )
                        )
                    )

                Browser.External link ->
                    ( Model rec
                    , Browser.Navigation.pushUrl rec.navigationKey link
                    )

        ResponseTimeZone timeZoneAndName ->
            ( Model { rec | subModel = Message.setTimeZoneAndName timeZoneAndName rec.subModel }
            , Cmd.none
            )

        CreateProjectResponse projectAndIdMaybe ->
            let
                ( newPageModel, pageCommand ) =
                    pageInit Data.LocationHome
            in
            ( Model
                { rec
                    | page = newPageModel
                    , eventList =
                        rec.eventList
                            ++ [ case projectAndIdMaybe of
                                    Just projectAndId ->
                                        CreatedProject projectAndId

                                    Nothing ->
                                        CreateProjectFailed
                               ]
                }
            , commandToMainCommand
                (Message.getLogInState rec.subModel)
                (Message.Batch
                    [ pageCommand
                    , case projectAndIdMaybe of
                        Just projectAndId ->
                            Message.GetBlobUrl projectAndId.snapshot.imageHash

                        Nothing ->
                            Message.None
                    ]
                )
            )

        CreateIdeaResponse ideaSnapshotAndIdMaybe ->
            case ideaSnapshotAndIdMaybe of
                Just ideaSnapshotAndId ->
                    let
                        ( newPageModel, command ) =
                            pageInit (Data.LocationIdea ideaSnapshotAndId.id)
                    in
                    ( Model { rec | page = newPageModel }
                    , commandToMainCommand (Message.getLogInState rec.subModel) command
                    )

                Nothing ->
                    ( Model rec
                    , Cmd.none
                    )

        UpdateTime timePosix ->
            commonMessageUpdate
                Message.UpdateTime
                (Model { rec | subModel = Message.setNowTime timePosix rec.subModel })

        NotificationDeleteAt index ->
            let
                eventListAsArray =
                    rec.eventList |> Array.fromList
            in
            ( Model
                { rec
                    | eventList =
                        Array.toList
                            (Array.append
                                (Array.slice 0 (max 0 index) eventListAsArray)
                                (Array.slice (index + 1) (Array.length eventListAsArray) eventListAsArray)
                            )
                }
            , Cmd.none
            )

        KeyPressed key ->
            case
                keyToCommonMessage
                    (case rec.page of
                        Suggestion model ->
                            Page.Suggestion.getBrowserUiState model

                        _ ->
                            Message.NotFocus
                    )
                    key
            of
                Just commonCommand ->
                    commonMessageUpdate commonCommand (Model rec)

                Nothing ->
                    ( Model rec
                    , Cmd.none
                    )

        CommonMessage commonMessage ->
            commonMessageUpdate commonMessage (Model rec)

        NoOperation ->
            ( Model rec
            , Cmd.none
            )


mapPageModel : ( pageModel, Message.Command ) -> (pageModel -> PageModel) -> Model -> ( Model, Cmd Msg )
mapPageModel ( newPageModel, command ) pageModelFunction (Model record) =
    ( Model { record | page = pageModelFunction newPageModel }
    , commandToMainCommand (Message.getLogInState record.subModel) command
    )


updatePage : PageMessage -> Model -> ( PageModel, Message.Command )
updatePage pageMessage (Model record) =
    case ( record.page, pageMessage ) of
        ( Home model, PageMessageHome message ) ->
            Page.Home.update message model
                |> Tuple.mapFirst Home

        ( CreateProject model, PageMessageCreateProject message ) ->
            Page.CreateProject.update message model
                |> Tuple.mapFirst CreateProject

        ( CreateIdea model, PageMessageCreateIdea message ) ->
            Page.CreateIdea.update message model
                |> Tuple.mapFirst CreateIdea

        ( Project model, PageMessageProject message ) ->
            Page.Project.update message model
                |> Tuple.mapFirst Project

        ( User model, PageMessageUser message ) ->
            Page.User.update message model
                |> Tuple.mapFirst User

        ( Idea model, PageMessageIdea message ) ->
            Page.Idea.update message model
                |> Tuple.mapFirst Idea

        ( Suggestion model, PageMessageSuggestion message ) ->
            Page.Suggestion.update message model
                |> Tuple.mapFirst Suggestion

        ( _, _ ) ->
            ( record.page
            , Message.None
            )


pageModelToLocation : PageModel -> Data.Location
pageModelToLocation pageModel =
    case pageModel of
        Home _ ->
            Data.LocationHome

        CreateProject _ ->
            Data.LocationCreateProject

        CreateIdea model ->
            Data.LocationCreateIdea (Page.CreateIdea.getProjectId model)

        Project model ->
            Data.LocationProject (Page.Project.getProjectId model)

        User model ->
            Data.LocationUser (Page.User.getUserId model)

        Idea model ->
            Data.LocationIdea (Page.Idea.getIdeaId model)

        Suggestion model ->
            Data.LocationSuggestion (Page.Suggestion.getSuggestionId model)


updateFromMsgList : List Msg -> Model -> ( Model, Cmd Msg )
updateFromMsgList msgList model =
    case msgList of
        msg :: tailMsg ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            updateFromMsgList tailMsg newModel
                |> Tuple.mapSecond (\next -> Cmd.batch [ cmd, next ])

        [] ->
            ( model, Cmd.none )


commonMessageUpdate : Message.CommonMessage -> Model -> ( Model, Cmd Msg )
commonMessageUpdate commonMessage (Model record) =
    case record.page of
        Home pageModel ->
            mapPageModel
                (Page.Home.updateByCommonMessage commonMessage pageModel)
                Home
                (Model record)

        Project pageModel ->
            mapPageModel
                (Page.Project.updateByCommonMessage commonMessage pageModel)
                Project
                (Model record)

        User pageModel ->
            mapPageModel
                (Page.User.updateByCommonMessage commonMessage pageModel)
                User
                (Model record)

        Idea pageModel ->
            mapPageModel
                (Page.Idea.updateByCommonMessage record.subModel commonMessage pageModel)
                Idea
                (Model record)

        Suggestion pageModel ->
            mapPageModel
                (Page.Suggestion.updateByCommonMessage commonMessage pageModel)
                Suggestion
                (Model record)

        CreateProject pageModel ->
            mapPageModel
                (Page.CreateProject.updateByCommonMessage commonMessage pageModel)
                CreateProject
                (Model record)

        CreateIdea pageModel ->
            mapPageModel
                (Page.CreateIdea.updateByCommonMessage commonMessage pageModel)
                CreateIdea
                (Model record)



{- =================================================
                       キー入力
   =================================================
-}
{- =================================================
                       マウス入力
   =================================================
-}


{-| マウスのボタンを離した、タッチを離した
-}
pointerUp : Model -> ( Model, Cmd Msg )
pointerUp model =
    ( model, Cmd.none )



{- ============ Tree Panel ============= -}


isCaptureMouseEvent : Model -> Bool
isCaptureMouseEvent model =
    getGutterType model /= Nothing


getGutterType : Model -> Maybe GutterType
getGutterType (Model { subMode }) =
    case subMode of
        SubModeNone ->
            Nothing

        SubModeGutter gutter ->
            Just gutter


toGutterMode : GutterType -> Model -> Model
toGutterMode gutter (Model rec) =
    Model
        { rec
            | subMode = SubModeGutter gutter
        }



{- ============ キー入力されたら、すぐpreventDefaultしないとだめなため、後で処理するmsgを入れとく =============== -}


pushMsgListToMsgQueue : List Msg -> Model -> Model
pushMsgListToMsgQueue msgList (Model rec) =
    Model
        { rec
            | messageQueue = rec.messageQueue ++ msgList
        }


shiftMsgListFromMsgQueue : Model -> ( List Msg, Model )
shiftMsgListFromMsgQueue (Model rec) =
    ( rec.messageQueue
    , Model
        { rec | messageQueue = [] }
    )


requestLogInUrlTyped : Data.RequestLogInUrlRequestData -> Cmd Msg
requestLogInUrlTyped requestLogInUrlRequestData =
    requestLogInUrl
        (Data.requestLogInUrlRequestDataToJsonValue requestLogInUrlRequestData)


getUserByAccessTokenTyped : Data.AccessToken -> Cmd Msg
getUserByAccessTokenTyped accessToken =
    getUserByAccessToken
        (Data.accessTokenToJsonValue accessToken)


getImageBlobUrlTyped : Data.ImageToken -> Cmd Msg
getImageBlobUrlTyped fileHash =
    getImageBlobUrl
        (Data.imageTokenToJsonValue fileHash)


createProjectTyped : Data.CreateProjectParameter -> Cmd Msg
createProjectTyped createProjectParameter =
    createProject
        (Data.createProjectParameterToJsonValue createProjectParameter)


createIdeaTyped : Data.CreateIdeaParameter -> Cmd Msg
createIdeaTyped createIdeaParameter =
    createIdea
        (Data.createIdeaParameterToJsonValue createIdeaParameter)



{- ================================================================
                               View
   ================================================================
-}


{-| 見た目を定義する
-}
view : Model -> Browser.Document Msg
view (Model rec) =
    { title = "Definy"
    , body =
        [ Ui.depth
            Ui.stretch
            Ui.stretch
            (case getGutterType (Model rec) of
                Just gutterType ->
                    [ Ui.pointerImage (gutterTypeToCursorStyle gutterType) ]

                Nothing ->
                    []
            )
            [ ( ( Ui.Center, Ui.Center )
              , mainView (Model rec)
              )
            , ( ( Ui.End, Ui.End )
              , notificationView rec.subModel rec.eventList
              )
            ]
            |> Ui.toHtml
            |> Html.Styled.toUnstyled
        ]
    }


mainView : Model -> Ui.Panel Msg
mainView (Model record) =
    case Message.getLogInState record.subModel of
        Data.LogInState.RequestLogInUrl _ ->
            Ui.depth
                Ui.stretch
                Ui.stretch
                []
                [ ( ( Ui.Center, Ui.Center )
                  , CommonUi.normalText 24 "ログインを準備中……"
                  )
                ]

        _ ->
            mainContentView (Model record)


mainContentView : Model -> Ui.Panel Msg
mainContentView (Model record) =
    Ui.scroll Ui.stretch
        Ui.stretch
        []
        (case record.page of
            Home pageModel ->
                Page.Home.view record.subModel pageModel
                    |> Ui.map (PageMessageHome >> PageMsg)

            CreateProject pageModel ->
                Page.CreateProject.view record.subModel pageModel
                    |> Ui.map (PageMessageCreateProject >> PageMsg)

            CreateIdea pageModel ->
                Page.CreateIdea.view record.subModel pageModel
                    |> Ui.map (PageMessageCreateIdea >> PageMsg)

            Project pageModel ->
                Page.Project.view record.subModel pageModel
                    |> Ui.map (PageMessageProject >> PageMsg)

            User pageModel ->
                Page.User.view record.subModel pageModel
                    |> Ui.map (PageMessageUser >> PageMsg)

            Idea pageModel ->
                Page.Idea.view
                    record.subModel
                    pageModel
                    |> Ui.map (PageMessageIdea >> PageMsg)

            Suggestion pageModel ->
                Page.Suggestion.view
                    record.subModel
                    pageModel
                    |> Ui.map (PageMessageSuggestion >> PageMsg)
        )


notificationView :
    Message.SubModel
    -> List Event
    -> Ui.Panel Msg
notificationView subModel eventList =
    Ui.column
        (Ui.fix 512)
        Ui.auto
        [ Ui.gap 8
        , Ui.padding 16
        ]
        (List.indexedMap
            (\index event ->
                cardItem index (eventToCardStyle subModel event)
            )
            eventList
        )


eventToCardStyle : Message.SubModel -> Event -> CardStyle
eventToCardStyle subModel event =
    case event of
        LogInSuccess userAndUserId ->
            CardStyle
                { icon =
                    Message.getImageBlobUrl userAndUserId.snapshot.imageHash subModel
                        |> Maybe.map
                            (\blobUrl ->
                                Icon
                                    { alternativeText =
                                        userAndUserId.snapshot.name ++ "のプロフィール画像"
                                    , url = blobUrl
                                    }
                            )
                , text = "\"" ++ userAndUserId.snapshot.name ++ "\"としてログインしました"
                }

        LogInFailure ->
            CardStyle
                { icon = Nothing
                , text = "ログイン失敗"
                }

        OnLine ->
            CardStyle
                { icon = Nothing
                , text = "オンラインになりました"
                }

        OffLine ->
            CardStyle
                { icon = Nothing
                , text = "オフラインになりました"
                }

        CreatedProject projectAndId ->
            CardStyle
                { icon =
                    Message.getImageBlobUrl projectAndId.snapshot.imageHash subModel
                        |> Maybe.map
                            (\blobUrl ->
                                Icon
                                    { alternativeText = projectAndId.snapshot.name ++ "のアイコン"
                                    , url = blobUrl
                                    }
                            )
                , text = projectAndId.snapshot.name ++ "を作成しました"
                }

        CreateProjectFailed ->
            CardStyle
                { icon = Nothing
                , text = "プロジェクトの作成に失敗しました"
                }


type Icon
    = Icon
        { alternativeText : String
        , url : String
        }


type CardStyle
    = CardStyle
        { icon : Maybe Icon
        , text : String
        }


cardItem : Int -> CardStyle -> Ui.Panel Msg
cardItem index (CardStyle record) =
    Ui.row
        Ui.stretch
        (Ui.fix 48)
        [ Ui.backgroundColor (Css.rgb 0 100 0) ]
        [ case record.icon of
            Just (Icon icon) ->
                Ui.bitmapImage
                    (Ui.fix 48)
                    (Ui.fix 48)
                    [ Ui.padding 4 ]
                    (Ui.BitmapImageAttributes
                        { blobUrl = icon.url
                        , fitStyle = Ui.Contain
                        , alternativeText = icon.alternativeText
                        , rendering = Ui.ImageRenderingAuto
                        }
                    )

            Nothing ->
                Ui.empty (Ui.fix 32) (Ui.fix 32) []
        , CommonUi.stretchText
            16
            record.text
        , Ui.button (Ui.fix 32) (Ui.fix 32) [] (NotificationDeleteAt index) CommonUi.closeIcon
        ]


gutterTypeToCursorStyle : GutterType -> Ui.PointerImage
gutterTypeToCursorStyle gutterType =
    case gutterType of
        GutterTypeVertical ->
            Ui.HorizontalResize

        GutterTypeHorizontal ->
            Ui.VerticalResize


responseUserDataFromAccessToken : Maybe Data.UserSnapshotAndId -> Model -> ( Model, Cmd Msg )
responseUserDataFromAccessToken userSnapshotAndIdMaybe (Model rec) =
    case ( userSnapshotAndIdMaybe, Message.getLogInState rec.subModel ) of
        ( Just userSnapshotAndId, Data.LogInState.VerifyingAccessToken accessToken ) ->
            ( Model
                { rec
                    | subModel =
                        Message.setLogInState
                            (Data.LogInState.Ok
                                { accessToken = accessToken
                                , userSnapshotAndId = userSnapshotAndId
                                }
                            )
                            rec.subModel
                    , eventList = rec.eventList ++ [ LogInSuccess userSnapshotAndId ]
                }
            , Cmd.batch
                [ consoleLog "ユーザー情報の取得に成功!"
                , commandToMainCommand (Message.getLogInState rec.subModel)
                    (Message.GetBlobUrl userSnapshotAndId.snapshot.imageHash)
                ]
            )

        ( Nothing, Data.LogInState.VerifyingAccessToken _ ) ->
            ( Model
                { rec
                    | subModel = Message.setLogInState Data.LogInState.GuestUser rec.subModel
                    , eventList = rec.eventList ++ [ LogInFailure ]
                }
            , Cmd.none
            )

        ( _, _ ) ->
            ( Model rec
            , consoleLog "いらないときにユーザーの情報を受け取ってしまった"
            )


commandToMainCommand : Data.LogInState.LogInState -> Message.Command -> Cmd Msg
commandToMainCommand logInState command =
    case command of
        Message.RequestLogInUrl provider ->
            Task.succeed (RequestLogInUrl provider)
                |> Task.perform identity

        Message.None ->
            Cmd.none

        Message.GetBlobUrl fileHash ->
            getImageBlobUrlTyped fileHash

        Message.CreateProject projectName ->
            case logInState of
                Data.LogInState.Ok { accessToken } ->
                    createProjectTyped
                        { projectName = projectName
                        , accessToken = accessToken
                        }

                _ ->
                    Cmd.none

        Message.CreateIdea projectIdAndIdeaName ->
            case logInState of
                Data.LogInState.Ok { accessToken } ->
                    createIdeaTyped
                        { accessToken = accessToken, ideaName = projectIdAndIdeaName.ideaName, projectId = projectIdAndIdeaName.projectId }

                _ ->
                    Cmd.none

        Message.AddComment ideaIdAndComment ->
            case logInState of
                Data.LogInState.Ok { accessToken } ->
                    addComment
                        (Data.addCommentParameterToJsonValue
                            { ideaId = ideaIdAndComment.ideaId
                            , comment = ideaIdAndComment.comment
                            , accessToken = accessToken
                            }
                        )

                _ ->
                    Cmd.none

        Message.AddSuggestion ideaId ->
            case logInState of
                Data.LogInState.Ok { accessToken } ->
                    addSuggestion
                        (Data.addSuggestionParameterToJsonValue
                            { ideaId = ideaId
                            , accessToken = accessToken
                            }
                        )

                _ ->
                    Cmd.none

        Message.ConsoleLog string ->
            consoleLog string

        Message.PushUrl urlData ->
            Task.perform
                identity
                (Task.succeed (OnUrlRequest (Browser.Internal (Data.UrlData.urlDataToUrl urlData))))

        Message.ToValidProjectName string ->
            toValidProjectName string

        Message.ToValidIdeaName string ->
            toValidIdeaName string

        Message.ToValidTypePartName name ->
            toValidTypePartName name

        Message.GetUser userId ->
            getUser (Data.userIdToJsonValue userId)

        Message.GetUserNoCache userId ->
            getUserNoCache (Data.userIdToJsonValue userId)

        Message.GetAllProjectId ->
            getAllProjectIdList ()

        Message.GetProject projectId ->
            getProject (Data.projectIdToJsonValue projectId)

        Message.GetProjectNoCache projectId ->
            getProjectNoCache (Data.projectIdToJsonValue projectId)

        Message.GetIdea ideaId ->
            getIdea (Data.ideaIdToJsonValue ideaId)

        Message.GetIdeaNoCache ideaId ->
            getIdeaNoCache (Data.ideaIdToJsonValue ideaId)

        Message.GetSuggestion suggestionId ->
            getSuggestion (Data.suggestionIdToJsonValue suggestionId)

        Message.GetSuggestionNoCache suggestionId ->
            getSuggestionNoCache (Data.suggestionIdToJsonValue suggestionId)

        Message.GetIdeaListByProjectId projectId ->
            getIdeaAndIdListByProjectId (Data.projectIdToJsonValue projectId)

        Message.FocusElement elementId ->
            Task.attempt (always NoOperation) (Browser.Dom.focus elementId)

        Message.Batch commandList ->
            Cmd.batch (List.map (commandToMainCommand logInState) commandList)



{- ================================================================
                           Subscription
   ================================================================
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ keyPressed
            (\jsonValue ->
                case Data.Key.fromKeyEventObject jsonValue of
                    Just key ->
                        KeyPressed key

                    Nothing ->
                        NoOperation
            )
         , keyPrevented (always KeyPrevented)
         , windowResize WindowResize
         , changeNetworkConnection ChangeNetworkConnection
         , Time.every 1000 UpdateTime
         , responseUserByAccessToken
            (\jsonValue ->
                case
                    Json.Decode.decodeValue
                        (Data.maybeJsonDecoder Data.userSnapshotAndIdJsonDecoder)
                        jsonValue
                of
                    Ok userSnapshotAndIdMaybe ->
                        ResponseUserDataFromAccessToken userSnapshotAndIdMaybe

                    Err _ ->
                        NoOperation
            )
         , getImageBlobResponse
            (\{ blobUrl, imageToken } ->
                ResponseImageBlob
                    { blobUrl = blobUrl
                    , imageToken = Data.ImageToken imageToken
                    }
            )
         , toValidProjectNameResponse
            (\response ->
                CommonMessage
                    (Message.ToValidProjectNameResponse response)
            )
         , toValidIdeaNameResponse
            (\response ->
                CommonMessage
                    (Message.ToValidIdeaNameResponse response)
            )
         , toValidTypePartNameResponse
            (\response ->
                CommonMessage
                    (Message.ToValidTypePartNameResponse response)
            )
         , createProjectResponse
            (\jsonValue ->
                case
                    Json.Decode.decodeValue
                        (Data.maybeJsonDecoder
                            Data.projectSnapshotAndIdJsonDecoder
                        )
                        jsonValue
                of
                    Ok projectAndIdMaybe ->
                        CreateProjectResponse projectAndIdMaybe

                    Err _ ->
                        NoOperation
            )
         , responseCreateIdea
            (\jsonValue ->
                case Json.Decode.decodeValue (Data.maybeJsonDecoder Data.ideaSnapshotAndIdJsonDecoder) jsonValue of
                    Ok ideaSnapshotMaybe ->
                        CreateIdeaResponse ideaSnapshotMaybe

                    Err _ ->
                        NoOperation
            )
         , responseAddComment
            (\jsonValue ->
                case
                    Json.Decode.decodeValue Data.ideaResponseJsonDecoder jsonValue
                of
                    Ok ideaSnapshotMaybe ->
                        CommonMessage (Message.ResponseIdea ideaSnapshotMaybe)

                    Err _ ->
                        NoOperation
            )
         , responseAddSuggestion
            (\jsonValue ->
                case Json.Decode.decodeValue (Data.maybeJsonDecoder Data.suggestionSnapshotAndIdJsonDecoder) jsonValue of
                    Ok suggestionSnapshotAndIdMaybe ->
                        CommonMessage (Message.ResponseAddSuggestion suggestionSnapshotAndIdMaybe)

                    Err _ ->
                        NoOperation
            )
         , responseAllProjectId
            (\jsonValue ->
                case
                    Json.Decode.decodeValue
                        (Json.Decode.list
                            Data.projectIdJsonDecoder
                        )
                        jsonValue
                of
                    Ok projectIdList ->
                        CommonMessage (Message.ResponseAllProjectIdList projectIdList)

                    Err _ ->
                        NoOperation
            )
         , responseProject
            (\jsonValue ->
                case
                    Json.Decode.decodeValue
                        Data.projectResponseJsonDecoder
                        jsonValue
                of
                    Ok projectWithIdAndRespondTimeMaybe ->
                        CommonMessage (Message.ResponseProject projectWithIdAndRespondTimeMaybe)

                    Err _ ->
                        NoOperation
            )
         , responseUser
            (\jsonValue ->
                case Json.Decode.decodeValue Data.userResponseJsonDecoder jsonValue of
                    Ok userSnapshotMaybeAndId ->
                        ResponseUser userSnapshotMaybeAndId

                    Err _ ->
                        NoOperation
            )
         , responseIdea
            (\jsonValue ->
                case Json.Decode.decodeValue Data.ideaResponseJsonDecoder jsonValue of
                    Ok ideaResponse ->
                        CommonMessage (Message.ResponseIdea ideaResponse)

                    Err _ ->
                        NoOperation
            )
         , responseSuggestion
            (\jsonValue ->
                case Json.Decode.decodeValue Data.suggestionResponseJsonDecoder jsonValue of
                    Ok suggestionResponse ->
                        CommonMessage (Message.ResponseSuggestion suggestionResponse)

                    Err _ ->
                        NoOperation
            )
         , responseIdeaSnapshotAndIdListByProjectId
            (\jsonValue ->
                case
                    Json.Decode.decodeValue Data.ideaListByProjectIdResponseJsonDecoder jsonValue
                of
                    Ok ideaList ->
                        CommonMessage (Message.ResponseIdeaListByProjectId ideaList)

                    Err _ ->
                        NoOperation
            )
         ]
            ++ (if isCaptureMouseEvent model then
                    [ subPointerUp (always PointerUp) ]

                else
                    []
               )
        )


keyToCommonMessage : Message.BrowserUiState -> Data.Key.Key -> Maybe Message.CommonMessage
keyToCommonMessage browserUiState key =
    case ( browserUiState, key.key ) of
        ( Message.NotFocus, Data.Key.KeyW ) ->
            Just Message.SelectUp

        ( Message.NotFocus, Data.Key.ArrowUp ) ->
            Just Message.SelectUp

        ( Message.NotFocus, Data.Key.KeyS ) ->
            Just Message.SelectDown

        ( Message.NotFocus, Data.Key.ArrowDown ) ->
            Just Message.SelectDown

        ( Message.NotFocus, Data.Key.KeyA ) ->
            Just Message.SelectLeft

        ( Message.NotFocus, Data.Key.ArrowLeft ) ->
            Just Message.SelectLeft

        ( Message.NotFocus, Data.Key.KeyD ) ->
            Just Message.SelectRight

        ( Message.NotFocus, Data.Key.ArrowRight ) ->
            Just Message.SelectRight

        ( Message.NotFocus, Data.Key.KeyE ) ->
            Just Message.SelectFirstChild

        ( _, Data.Key.Space ) ->
            Just Message.SelectFirstChild

        ( Message.NotFocus, Data.Key.KeyQ ) ->
            Just Message.SelectParent

        ( _, Data.Key.Enter ) ->
            Just Message.SelectParent

        ( _, Data.Key.Escape ) ->
            Just Message.SelectParent

        ( Message.NotFocus, Data.Key.KeyN ) ->
            Just Message.NewElement

        _ ->
            Nothing
