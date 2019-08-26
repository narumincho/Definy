port module Model exposing
    ( GutterType(..)
    , Model
    , Msg
    , editorPanelMsgToMsg
    , editorPanelUpdate
    , focusToEditorGroupPanel
    , focusToTreePanel
    , getActiveEditor
    , getCommandPaletteModel
    , getCurrentUser
    , getEditorGroupPanelGutter
    , getEditorGroupPanelModel
    , getEditorGroupPanelSize
    , getGutterType
    , getLanguage
    , getProject
    , getSidePanelModel
    , getTreePanelWidth
    , init
    , isFocusEditorGroupPanel
    , isFocusTreePanel
    , isTreePanelGutter
    , onUrlChange
    , onUrlRequest
    , sidePanelMsgToMsg
    , subscriptions
    , toTreePanelGutterMode
    , update
    )

{-| すべての状態を管理する
-}

import Browser
import Browser.Events
import Data.Key
import Data.Language
import Data.Project
import Data.SocialLoginService
import Data.User
import Json.Decode
import Panel.CommandPalette
import Panel.DefaultUi
import Panel.Editor.Module
import Panel.EditorGroup
import Panel.EditorItemSource
import Panel.Side
import Url
import Utility.ListExtra
import Utility.Map



{- Cmd (Elm → JavaScript) -}


port setTextAreaValue : String -> Cmd msg


port setClickEventListenerInCapturePhase : String -> Cmd msg


port focusTextArea : () -> Cmd msg


port preventDefaultBeforeKeyEvent : () -> Cmd msg


port elementScrollIntoView : String -> Cmd msg


port logInWithGoogle : () -> Cmd msg


port logInWithGitHub : () -> Cmd msg


port logInWithLine : () -> Cmd msg



{- Sub (JavaScript → Elm) -}


port keyPressed : (Json.Decode.Value -> msg) -> Sub msg


port keyPrevented : (() -> msg) -> Sub msg


port windowResize : ({ width : Int, height : Int } -> msg) -> Sub msg


port runResult : ({ ref : List Int, index : Int, result : Int } -> msg) -> Sub msg


port fireClickEventInCapturePhase : (String -> msg) -> Sub msg


port changeLanguage : (String -> msg) -> Sub msg


{-| 全体の入力を表すメッセージ
-}
type Msg
    = KeyPressed (Maybe Data.Key.Key) -- キーボードから入力
    | KeyPrevented -- キーボードの入力のデフォルト動作を取り消した後
    | MouseMove { x : Int, y : Int } -- マウスの移動
    | MouseUp -- マウスのボタンを離した
    | FireClickEventInCapturePhase String -- 外側から発生するクリックイベントを受け取った
    | ReceiveRunResult
        { ref : List Int
        , index : Int
        , result : Int
        }
      -- 実行結果を受け取った
    | ToResizeGutterMode Gutter -- リサイズモードに移行
    | CloseSidePanel -- サイドパネルを閉じる
    | FocusTo Focus -- フォーカスを移動
    | WindowResize { width : Int, height : Int } -- ウィンドウサイズを変更
    | SidePanelMsg Panel.Side.Msg -- ツリーパネル独自のメッセージ
    | EditorPanelMsg Panel.EditorGroup.Msg -- エディタパネル独自のメッセージ
    | ChangeEditorResource Panel.EditorItemSource.EditorItemSource -- エディタの対象を変える
    | OpenCommandPalette -- コマンドパレットを開く
    | CloseCommandPalette -- コマンドパレッドを閉じる
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url.Url
    | LogOutRequest -- ログアウトを要求する
    | LogInRequest Data.SocialLoginService.SocialLoginService
    | ChangeLanguage String -- 使用言語が変わった


{-| 全体を表現する
-}
type Model
    = Model
        { project : Data.Project.Project
        , focus : Focus
        , subMode : SubMode
        , sidePanelModel : Panel.Side.Model
        , editorGroupPanelModel : Panel.EditorGroup.Model
        , sidePanelWidth : Int
        , windowSize : { width : Int, height : Int }
        , msgQueue : List Msg
        , user : Maybe Data.User.User
        , language : Data.Language.Language
        }


{-| フォーカスしているものとフォーカス時に持てる状態を表す
-}
type Focus
    = FocusTreePanel
    | FocusEditorGroupPanel


type SubMode
    = SubModeNone
    | SubModeCommandPalette Panel.CommandPalette.Model
    | SubModeGutter Gutter


{-| パネルのサイズを変えるときにつかむ、Gutterの種類を表す
-}
type Gutter
    = SideBarGutter
    | GutterEditorGroupPanelVertical Panel.EditorGroup.GutterVertical
    | GutterEditorGroupPanelHorizontal Panel.EditorGroup.GutterHorizontal



{- Gutterの種類 -}


type GutterType
    = GutterTypeVertical
    | GutterTypeHorizontal


init :
    { url : String
    , user : Maybe { name : String, imageUrl : String, token : String }
    , language : String
    }
    -> ( Model, Cmd Msg )
init { user, language } =
    let
        ( editorPanelModel, emitListFromEditorGroupPanel ) =
            Panel.EditorGroup.initModel

        ( msgFromEditorGroupPanel, cmdFromEditorGroupPanel ) =
            emitListFromEditorGroupPanel
                |> List.map editorPanelEmitToMsg
                |> Utility.ListExtra.listTupleListToTupleList

        model =
            Model
                { project = Data.Project.sample
                , focus = FocusEditorGroupPanel
                , subMode = SubModeNone
                , sidePanelModel = Panel.Side.initModel
                , editorGroupPanelModel = editorPanelModel
                , sidePanelWidth = 250
                , windowSize = { width = 0, height = 0 }
                , msgQueue = []
                , user =
                    user
                        |> Maybe.map
                            (\{ name, imageUrl } ->
                                Data.User.fromName name
                            )
                , language = Data.Language.languageFromString language
                }
    in
    updateFromList
        msgFromEditorGroupPanel
        model
        |> Tuple.mapSecond
            (\c ->
                Cmd.batch
                    (cmdFromEditorGroupPanel ++ [ c ])
            )



{- =============== Msg ================ -}


toTreePanelGutterMode : Msg
toTreePanelGutterMode =
    ToResizeGutterMode SideBarGutter


{-| サイドパネルのMsgを全体のMsgに変換する
-}
sidePanelMsgToMsg : Panel.Side.Msg -> Msg
sidePanelMsgToMsg =
    SidePanelMsg


{-| エディタパネルのMsgを全体のMsgに変換する
-}
editorPanelMsgToMsg : Panel.EditorGroup.Msg -> Msg
editorPanelMsgToMsg =
    EditorPanelMsg


{-| ツリーパネルにフォーカスをあわせる
-}
focusToTreePanel : Msg
focusToTreePanel =
    FocusTo FocusTreePanel


{-| エディタグループパネルにフォーカスをあわせる
-}
focusToEditorGroupPanel : Msg
focusToEditorGroupPanel =
    FocusTo FocusEditorGroupPanel


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    OnUrlRequest


onUrlChange : Url.Url -> Msg
onUrlChange =
    OnUrlChange



{- ============================================
                   Update
   ============================================
-}


{-| Definy全体のUpdate
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressed key ->
            case keyDown key model of
                [] ->
                    ( model, Cmd.none )

                concreteMsgList ->
                    ( model |> pushMsgListToMsgQueue concreteMsgList
                    , preventDefaultBeforeKeyEvent ()
                    )

        KeyPrevented ->
            let
                ( listMsg, newModel ) =
                    model |> shiftMsgListFromMsgQueue
            in
            newModel |> updateFromList listMsg

        MouseMove position ->
            ( mouseMove position model
            , Cmd.none
            )

        MouseUp ->
            ( mouseUp model
            , Cmd.none
            )

        FireClickEventInCapturePhase idString ->
            model
                |> editorPanelUpdate
                    (Panel.EditorGroup.FireClickEventInCapturePhase idString)

        ReceiveRunResult data ->
            ( model
            , Cmd.none
            )

        ToResizeGutterMode gutter ->
            ( toGutterMode gutter model
            , Cmd.none
            )

        CloseSidePanel ->
            ( model |> setSidePanelWidth 0
            , Cmd.none
            )

        FocusTo focus ->
            setFocus focus model

        WindowResize { width, height } ->
            ( model |> setWindowSize { width = width, height = height }
            , Cmd.none
            )

        SidePanelMsg sidePanelMsg ->
            model |> sidePanelUpdate sidePanelMsg

        EditorPanelMsg editorPanelMsg ->
            model |> editorPanelUpdate editorPanelMsg

        ChangeEditorResource editorRef ->
            ( openEditor editorRef model
            , Cmd.none
            )

        OpenCommandPalette ->
            ( openCommandPalette model
            , Cmd.none
            )

        CloseCommandPalette ->
            ( closeCommandPalette model
            , Cmd.none
            )

        OnUrlRequest urlRequest ->
            ( model
            , Cmd.none
            )

        OnUrlChange url ->
            ( model
            , Cmd.none
            )

        LogOutRequest ->
            ( model
                |> singOut
            , Cmd.none
            )

        LogInRequest socialLoginService ->
            ( model
            , case socialLoginService of
                Data.SocialLoginService.Google ->
                    logInWithGoogle ()

                Data.SocialLoginService.GitHub ->
                    logInWithGitHub ()

                Data.SocialLoginService.Line ->
                    logInWithLine ()
            )

        ChangeLanguage string ->
            ( model |> setLanguage string
            , Cmd.none
            )


updateFromList : List Msg -> Model -> ( Model, Cmd Msg )
updateFromList msgList model =
    case msgList of
        msg :: tailMsg ->
            let
                ( newModel, cmd ) =
                    update msg model
            in
            updateFromList tailMsg newModel
                |> Tuple.mapSecond (\next -> Cmd.batch [ cmd, next ])

        [] ->
            ( model, Cmd.none )



{- =================================================
                       キー入力
   =================================================
-}


{-| キー入力をより具体的なMsgに変換する
-}
keyDown : Maybe Data.Key.Key -> Model -> List Msg
keyDown keyMaybe model =
    case keyMaybe of
        Just key ->
            case editorReservedKey (isOpenCommandPalette model) key of
                x :: xs ->
                    x :: xs

                [] ->
                    case isFocusDefaultUi model of
                        Just Panel.DefaultUi.MultiLineTextField ->
                            if multiLineTextFieldReservedKey key then
                                []

                            else
                                keyDownEachPanel key model

                        Just Panel.DefaultUi.SingleLineTextField ->
                            if singleLineTextFieldReservedKey key then
                                []

                            else
                                keyDownEachPanel key model

                        Nothing ->
                            keyDownEachPanel key model

        Nothing ->
            []


keyDownEachPanel : Data.Key.Key -> Model -> List Msg
keyDownEachPanel key model =
    case getFocus model of
        FocusTreePanel ->
            sidePanelKeyDown key
                |> List.map SidePanelMsg

        FocusEditorGroupPanel ->
            editorGroupPanelKeyDown key
                |> List.map EditorPanelMsg


{-| Definyによって予約されたキー。どのパネルにフォーカスが当たっていてもこれを優先する
-}
editorReservedKey : Bool -> Data.Key.Key -> List Msg
editorReservedKey isOpenPalette { key, ctrl, alt, shift } =
    if isOpenPalette then
        case ( ctrl, shift, alt ) of
            ( False, False, False ) ->
                case key of
                    Data.Key.Escape ->
                        [ CloseCommandPalette ]

                    Data.Key.F1 ->
                        [ OpenCommandPalette ]

                    _ ->
                        []

            _ ->
                []

    else
        case ( ctrl, shift, alt ) of
            -- 開いているけどキー入力を無視するために必要
            ( False, False, False ) ->
                case key of
                    Data.Key.F1 ->
                        [ OpenCommandPalette ]

                    _ ->
                        []

            ( False, False, True ) ->
                case key of
                    Data.Key.Digit0 ->
                        [ FocusTo FocusTreePanel ]

                    Data.Key.Digit1 ->
                        [ FocusTo FocusEditorGroupPanel ]

                    _ ->
                        []

            _ ->
                []


{-|

<textarea>で入力したときに予約されているであろうキーならTrue、そうでないならFalse。
複数行入力を想定している
ブラウザやOSで予約されているであろう動作を邪魔させないためにある。
Model.isFocusTextAreaがTrueになったときにまずこれを優先する

-}
multiLineTextFieldReservedKey : Data.Key.Key -> Bool
multiLineTextFieldReservedKey { key, ctrl, alt, shift } =
    case ( ctrl, shift, alt ) of
        ( False, False, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    True

                Data.Key.ArrowRight ->
                    True

                Data.Key.ArrowUp ->
                    True

                Data.Key.ArrowDown ->
                    True

                Data.Key.Enter ->
                    True

                Data.Key.Backspace ->
                    True

                _ ->
                    False

        ( True, False, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    True

                Data.Key.ArrowRight ->
                    True

                Data.Key.ArrowUp ->
                    True

                Data.Key.ArrowDown ->
                    True

                Data.Key.Backspace ->
                    True

                _ ->
                    False

        ( False, True, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    True

                Data.Key.ArrowRight ->
                    True

                Data.Key.ArrowUp ->
                    True

                Data.Key.ArrowDown ->
                    True

                _ ->
                    False

        ( True, True, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    True

                Data.Key.ArrowRight ->
                    True

                Data.Key.ArrowUp ->
                    True

                Data.Key.ArrowDown ->
                    True

                _ ->
                    False

        _ ->
            False


{-| <input type="text">で入力したときに予約されているであろうキーならTrue。そうでないなたFalse。
1行の入力を想定している
ブラウザやOSで予約されているであろう動作を邪魔させないためにある。
-}
singleLineTextFieldReservedKey : Data.Key.Key -> Bool
singleLineTextFieldReservedKey { key, ctrl, alt, shift } =
    case ( ctrl, shift, alt ) of
        ( False, False, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    True

                Data.Key.ArrowRight ->
                    True

                Data.Key.Backspace ->
                    True

                _ ->
                    False

        _ ->
            False



{- -------------------------------------------------
                 各パネルのキー入力
   -------------------------------------------------
-}


{-| サイドパネルのキー入力
-}
sidePanelKeyDown : Data.Key.Key -> List Panel.Side.Msg
sidePanelKeyDown { key, ctrl, shift, alt } =
    case ( ctrl, shift, alt ) of
        ( False, False, False ) ->
            case key of
                Data.Key.ArrowUp ->
                    [ Panel.Side.SelectUp ]

                Data.Key.ArrowDown ->
                    [ Panel.Side.SelectDown ]

                Data.Key.ArrowLeft ->
                    [ Panel.Side.SelectParentOrTreeClose ]

                Data.Key.ArrowRight ->
                    [ Panel.Side.SelectFirstChildOrTreeOpen ]

                Data.Key.Enter ->
                    [ Panel.Side.SelectItem ]

                _ ->
                    []

        _ ->
            []


{-| エディタグループパネルのキー入力
-}
editorGroupPanelKeyDown : Data.Key.Key -> List Panel.EditorGroup.Msg
editorGroupPanelKeyDown key =
    moduleEditorKeyMsg key
        |> List.map
            (Panel.EditorGroup.ModuleEditorMsg
                >> Panel.EditorGroup.EditorItemMsgToActive
            )


{-| モジュールエディタのキー入力
-}
moduleEditorKeyMsg : Data.Key.Key -> List Panel.Editor.Module.Msg
moduleEditorKeyMsg { key, ctrl, shift, alt } =
    case ( ctrl, shift, alt ) of
        ( False, False, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    [ Panel.Editor.Module.MsgActiveLeft ]

                Data.Key.ArrowRight ->
                    [ Panel.Editor.Module.MsgActiveRight ]

                Data.Key.ArrowUp ->
                    [ Panel.Editor.Module.MsgSuggestionPrevOrSelectUp ]

                Data.Key.ArrowDown ->
                    [ Panel.Editor.Module.MsgSuggestionNextOrSelectDown
                    ]

                Data.Key.Space ->
                    [ Panel.Editor.Module.MsgActiveToFirstChild ]

                Data.Key.Enter ->
                    [ Panel.Editor.Module.MsgConfirmSingleLineTextFieldOrSelectParent
                    ]

                _ ->
                    []

        ( True, False, False ) ->
            case key of
                Data.Key.ArrowLeft ->
                    [ Panel.Editor.Module.MsgActiveToLastChild ]

                Data.Key.ArrowRight ->
                    [ Panel.Editor.Module.MsgActiveToFirstChild ]

                Data.Key.Enter ->
                    [ Panel.Editor.Module.MsgConfirmMultiLineTextField ]

                _ ->
                    []

        ( False, False, True ) ->
            case key of
                Data.Key.ArrowUp ->
                    [ Panel.Editor.Module.MsgIncreaseValue ]

                Data.Key.ArrowDown ->
                    [ Panel.Editor.Module.MsgDecreaseValue ]

                _ ->
                    []

        _ ->
            []



{- =================================================
                       マウス入力
   =================================================
-}


{-| マウスを動かした
-}
mouseMove : { x : Int, y : Int } -> Model -> Model
mouseMove { x, y } model =
    case getGutterMode model of
        Just SideBarGutter ->
            model
                |> setSidePanelWidth
                    (treePanelResizeFromGutter (getWindowSize model).width x)

        Just (GutterEditorGroupPanelVertical gutter) ->
            model
                |> mapEditorGroupPanelModel
                    (Panel.EditorGroup.resizeFromVerticalGutter
                        { mouseRelX = max 0 (x - getTreePanelWidth model)
                        , editorWidth = (getWindowSize model).width - getTreePanelWidth model
                        }
                        gutter
                    )

        Just (GutterEditorGroupPanelHorizontal gutter) ->
            model
                |> mapEditorGroupPanelModel
                    (Panel.EditorGroup.resizeFromHorizontalGutter
                        { mouseRelY = max 0 y
                        , editorHeight = (getWindowSize model).height
                        }
                        gutter
                    )

        Nothing ->
            model


treePanelResizeFromGutter : Int -> Int -> Int
treePanelResizeFromGutter maxLimit x =
    if x < 80 then
        0

    else if x < 120 then
        120

    else
        min maxLimit x


{-| マウスのボタンを離した
-}
mouseUp : Model -> Model
mouseUp (Model rec) =
    Model
        { rec
            | subMode = SubModeNone
        }



{- ============ フォーカス 編集している要素について ============= -}


{-| フォーカスしている要素を取得する
-}
getFocus : Model -> Focus
getFocus (Model { focus }) =
    focus


{-| ツリーパネルにフォーカスが当たっているかどうか
-}
isFocusTreePanel : Model -> Bool
isFocusTreePanel model =
    case getFocus model of
        FocusTreePanel ->
            True

        FocusEditorGroupPanel ->
            False


{-| エディタグループパネルにフォーカスが当たっているかどうか
-}
isFocusEditorGroupPanel : Model -> Bool
isFocusEditorGroupPanel model =
    case getFocus model of
        FocusTreePanel ->
            False

        FocusEditorGroupPanel ->
            True


{-| フォーカスする要素を変更する。それによって発生するCmdもある
-}
setFocus : Focus -> Model -> ( Model, Cmd Msg )
setFocus focus (Model rec) =
    case focus of
        FocusTreePanel ->
            Model { rec | focus = FocusTreePanel }
                |> editorPanelUpdate Panel.EditorGroup.Blur

        FocusEditorGroupPanel ->
            Model { rec | focus = FocusEditorGroupPanel }
                |> editorPanelUpdate Panel.EditorGroup.Focus



{- ============ Tree Panel ============= -}


{-| ツリーパネルの幅を変更するモードかどうか
-}
isTreePanelGutter : Model -> Bool
isTreePanelGutter model =
    case getGutterMode model of
        Just SideBarGutter ->
            True

        _ ->
            False


getSidePanelModel : Model -> Panel.Side.Model
getSidePanelModel (Model { sidePanelModel }) =
    sidePanelModel


setSidePanelModel : Panel.Side.Model -> Model -> Model
setSidePanelModel sidePanelModel (Model rec) =
    Model
        { rec
            | sidePanelModel = sidePanelModel
        }


getCurrentUser : Model -> Maybe Data.User.User
getCurrentUser (Model { user }) =
    user


singOut : Model -> Model
singOut (Model rec) =
    Model
        { rec | user = Nothing }



{- ============ Editor Group Panel ============= -}


getEditorGroupPanelModel : Model -> Panel.EditorGroup.Model
getEditorGroupPanelModel (Model { editorGroupPanelModel }) =
    editorGroupPanelModel


setEditorGroupPanelModel : Panel.EditorGroup.Model -> Model -> Model
setEditorGroupPanelModel editorPanelModel (Model rec) =
    Model
        { rec
            | editorGroupPanelModel = editorPanelModel
        }


mapEditorGroupPanelModel : (Panel.EditorGroup.Model -> Panel.EditorGroup.Model) -> Model -> Model
mapEditorGroupPanelModel =
    Utility.Map.toMapper getEditorGroupPanelModel setEditorGroupPanelModel


{-| エディタグループパネルのGutterの状態を取得する
-}
getEditorGroupPanelGutter : Model -> Maybe Panel.EditorGroup.Gutter
getEditorGroupPanelGutter model =
    case getGutterMode model of
        Just (GutterEditorGroupPanelHorizontal gutter) ->
            Just (Panel.EditorGroup.GutterHorizontal gutter)

        Just (GutterEditorGroupPanelVertical gutter) ->
            Just (Panel.EditorGroup.GutterVertical gutter)

        _ ->
            Nothing



{- ============ パネルやウィンドウの幅高さ ============= -}


{-| ツリーパネルとエディタパネルの間にあるリサイズバーのX座標
-}
getVerticalGutterX : Model -> Int
getVerticalGutterX (Model { sidePanelWidth }) =
    sidePanelWidth


setSidePanelWidth : Int -> Model -> Model
setSidePanelWidth width (Model rec) =
    Model
        { rec
            | sidePanelWidth = width
        }


getTreePanelWidth : Model -> Int
getTreePanelWidth model =
    let
        width =
            getVerticalGutterX model - verticalGutterWidth // 2
    in
    if (getWindowSize model).width < 500 then
        0

    else if width < 120 then
        0

    else
        width


getEditorGroupPanelSize : Model -> { width : Int, height : Int }
getEditorGroupPanelSize model =
    { width = (getWindowSize model).width - (getTreePanelWidth model + verticalGutterWidth)
    , height = (getWindowSize model).height
    }


verticalGutterWidth : Int
verticalGutterWidth =
    2


getGutterType : Model -> Maybe GutterType
getGutterType model =
    getGutterMode model
        |> Maybe.map
            (\gutter ->
                case gutter of
                    SideBarGutter ->
                        GutterTypeVertical

                    GutterEditorGroupPanelVertical _ ->
                        GutterTypeVertical

                    GutterEditorGroupPanelHorizontal _ ->
                        GutterTypeHorizontal
            )


getWindowSize : Model -> { width : Int, height : Int }
getWindowSize (Model { windowSize }) =
    windowSize


setWindowSize : { width : Int, height : Int } -> Model -> Model
setWindowSize { width, height } (Model rec) =
    Model
        { rec
            | windowSize = { width = width, height = height }
        }


isCaptureMouseEvent : Model -> Bool
isCaptureMouseEvent model =
    getGutterMode model /= Nothing


getGutterMode : Model -> Maybe Gutter
getGutterMode (Model { subMode }) =
    case subMode of
        SubModeNone ->
            Nothing

        SubModeCommandPalette _ ->
            Nothing

        SubModeGutter gutter ->
            Just gutter


toGutterMode : Gutter -> Model -> Model
toGutterMode gutter (Model rec) =
    Model
        { rec
            | subMode = SubModeGutter gutter
        }


{-| サイドパネルの更新
-}
sidePanelUpdate : Panel.Side.Msg -> Model -> ( Model, Cmd Msg )
sidePanelUpdate msg model =
    let
        ( sidePanelModel, emitMsg ) =
            getSidePanelModel model
                |> Panel.Side.update
                    msg
    in
    model
        |> setSidePanelModel sidePanelModel
        |> updateFromList (emitMsg |> List.map sidePanelEmitToMsg)


{-| ツリーパネルで発生したEmitを全体のMsgに変換する
-}
sidePanelEmitToMsg : Panel.Side.Emit -> Msg
sidePanelEmitToMsg emit =
    case emit of
        Panel.Side.EmitLogOutRequest ->
            LogOutRequest

        Panel.Side.EmitLogInRequest service ->
            LogInRequest service


{-| エディタグループパネルの更新
-}
editorPanelUpdate : Panel.EditorGroup.Msg -> Model -> ( Model, Cmd Msg )
editorPanelUpdate msg model =
    let
        ( editorPanelModel, emitMsg ) =
            Panel.EditorGroup.update
                msg
                (getProject model)
                (getEditorGroupPanelModel model)

        ( nextMsg, cmd ) =
            emitMsg
                |> List.map editorPanelEmitToMsg
                |> Utility.ListExtra.listTupleListToTupleList
    in
    model
        |> setEditorGroupPanelModel editorPanelModel
        |> updateFromList nextMsg
        |> Tuple.mapSecond (\next -> Cmd.batch (cmd ++ [ next ]))


{-| エディタグループパネルの更新
-}
editorPanelEmitToMsg : Panel.EditorGroup.Emit -> ( List Msg, List (Cmd Msg) )
editorPanelEmitToMsg emit =
    case emit of
        Panel.EditorGroup.EmitVerticalGutterModeOn gutterVertical ->
            ( [ ToResizeGutterMode (GutterEditorGroupPanelVertical gutterVertical) ]
            , []
            )

        Panel.EditorGroup.EmitHorizontalGutterModeOn gutterHorizontal ->
            ( [ ToResizeGutterMode (GutterEditorGroupPanelHorizontal gutterHorizontal) ]
            , []
            )

        Panel.EditorGroup.EmitSetTextAreaValue string ->
            ( []
            , [ setTextAreaValue string ]
            )

        Panel.EditorGroup.EmitFocusEditTextAea ->
            ( []
            , [ focusTextArea () ]
            )

        Panel.EditorGroup.EmitSetClickEventListenerInCapturePhase idString ->
            ( []
            , [ setClickEventListenerInCapturePhase idString ]
            )

        Panel.EditorGroup.EmitElementScrollIntoView id ->
            ( []
            , [ elementScrollIntoView id ]
            )

        Panel.EditorGroup.EmitNone ->
            ( []
            , []
            )


{-| プロジェクトを取得する
-}
getProject : Model -> Data.Project.Project
getProject (Model { project }) =
    project


setProject : Data.Project.Project -> Model -> Model
setProject project (Model rec) =
    Model
        { rec | project = project }


{-| 開いているエディタを取得する
-}
getActiveEditor : Model -> Panel.EditorItemSource.EditorItemSource
getActiveEditor model =
    getEditorGroupPanelModel model
        |> Panel.EditorGroup.getActiveEditor


{-| エディタを開く
-}
openEditor : Panel.EditorItemSource.EditorItemSource -> Model -> Model
openEditor editorRef =
    mapEditorGroupPanelModel (Panel.EditorGroup.changeActiveEditorResource editorRef)



{- ====== コマンドパレット ====== -}


{-| コマンドパレットを開く
-}
openCommandPalette : Model -> Model
openCommandPalette (Model rec) =
    Model
        { rec
            | subMode = SubModeCommandPalette Panel.CommandPalette.initModel
        }


closeCommandPalette : Model -> Model
closeCommandPalette (Model rec) =
    Model
        { rec
            | subMode = SubModeNone
        }


{-| コマンドパレッドの状態を取得する
-}
getCommandPaletteModel : Model -> Maybe Panel.CommandPalette.Model
getCommandPaletteModel (Model { subMode }) =
    case subMode of
        SubModeNone ->
            Nothing

        SubModeGutter _ ->
            Nothing

        SubModeCommandPalette model ->
            Just model


isOpenCommandPalette : Model -> Bool
isOpenCommandPalette (Model { subMode }) =
    case subMode of
        SubModeNone ->
            False

        SubModeGutter _ ->
            False

        SubModeCommandPalette _ ->
            True


{-| いまブラウザが入力を受け取る要素にフォーカスが当たっているかどうか。当たっていたらブラウザのデフォルト動作を邪魔しない
-}
isFocusDefaultUi : Model -> Maybe Panel.DefaultUi.DefaultUi
isFocusDefaultUi model =
    case getFocus model of
        FocusTreePanel ->
            Nothing

        FocusEditorGroupPanel ->
            model
                |> getEditorGroupPanelModel
                |> Panel.EditorGroup.isFocusDefaultUi



{- ============ キー入力されたら、すぐpreventDefaultしないとだめなため、後で処理するmsgを入れとく =============== -}


pushMsgListToMsgQueue : List Msg -> Model -> Model
pushMsgListToMsgQueue msgList (Model rec) =
    Model
        { rec
            | msgQueue = rec.msgQueue ++ msgList
        }


shiftMsgListFromMsgQueue : Model -> ( List Msg, Model )
shiftMsgListFromMsgQueue (Model rec) =
    ( rec.msgQueue
    , Model
        { rec | msgQueue = [] }
    )


getLanguage : Model -> Data.Language.Language
getLanguage (Model { language }) =
    language


setLanguage : String -> Model -> Model
setLanguage string (Model rec) =
    Model
        { rec
            | language = Data.Language.languageFromString string
        }



{- ================================================================
                           Subscription
   ================================================================
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ([ keyPressed (Data.Key.fromKeyEventObject >> KeyPressed)
         , keyPrevented (always KeyPrevented)
         , windowResize WindowResize
         , runResult ReceiveRunResult
         , fireClickEventInCapturePhase FireClickEventInCapturePhase
         , changeLanguage ChangeLanguage
         ]
            ++ (if isCaptureMouseEvent model then
                    [ Browser.Events.onMouseMove
                        (Json.Decode.map2 (\x y -> MouseMove { x = x, y = y })
                            (Json.Decode.field "clientX" Json.Decode.int)
                            (Json.Decode.field "clientY" Json.Decode.int)
                        )
                    , Browser.Events.onMouseUp
                        (Json.Decode.succeed MouseUp)
                    , Browser.Events.onVisibilityChange
                        (always MouseUp)
                    ]

                else
                    []
               )
        )
