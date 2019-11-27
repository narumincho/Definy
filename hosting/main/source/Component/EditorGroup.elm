module Component.EditorGroup exposing
    ( Cmd(..)
    , EditorMsg(..)
    , Gutter(..)
    , GutterHorizontal
    , GutterVertical
    , Model
    , Msg(..)
    , changeActiveEditorResource
    , getActiveEditor
    , initModel
    , isFocusDefaultUi
    , resizeFromHorizontalGutter
    , resizeFromVerticalGutter
    , update
    , view
    )

{-| 画面に主に表示されるパネルで、左のツリーパネルで設定した内容を表示編集できる
また、編集画面を分割することもできる
-}

import Component.DefaultUi
import Component.Editor.EditorKeyConfig
import Component.Editor.Module
import Component.Editor.Project
import Component.Editor.ProjectImport
import Component.EditorItemSource
import Component.Style as Style
import Css
import Data.IdHash
import Data.Language
import Data.Project
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Utility.ListExtra
import Utility.Map
import VectorImage


{-| とりうる値を保持するModel
-}
type Model
    = Model
        { group : Group
        , activeEditorIndex : EditorIndex
        }


{-| エディタを複数持つグループ
-}
type Group
    = RowOne
        { left : ColumnGroup
        }
    | RowTwo
        { left : ColumnGroup
        , center : ColumnGroup
        , leftWidth : Int -- MAX 1000
        }
    | RowThree
        { left : ColumnGroup
        , center : ColumnGroup
        , right : ColumnGroup
        , leftWidth : Int
        , centerWidth : Int -- LeftとCenterを足してMAX 1000
        }


{-| 1列には1つか2つのエディタを持つ
-}
type ColumnGroup
    = ColumnOne
        { top : EditorModel
        }
    | ColumnTwo
        { top : EditorModel
        , bottom : EditorModel
        , topHeight : Int -- Max 1000
        }


{-| 各エディタのModelを保持する
-}
type EditorModel
    = ProjectEditor Component.Editor.Project.Model
    | ConfigEditor Component.Editor.ProjectImport.Model
    | ModuleEditor Component.Editor.Module.Model
    | EditorKeyConfig Component.Editor.EditorKeyConfig.Model


{-| 最大6個のエディタのどれを指しているのかを示す
-}
type alias EditorIndex =
    ( EditorIndexRow, EditorIndexColumn )


{-| 横方向。左、真ん中、右
-}
type EditorIndexRow
    = EditorIndexLeft
    | EditorIndexCenter
    | EditorIndexRight


{-| 縦方向。上、下
-}
type EditorIndexColumn
    = EditorIndexTop
    | EditorIndexBottom


{-| すべてのエディタの指し位置
-}
editorIndexAllValue : List EditorIndex
editorIndexAllValue =
    [ ( EditorIndexLeft, EditorIndexTop )
    , ( EditorIndexLeft, EditorIndexBottom )
    , ( EditorIndexCenter, EditorIndexTop )
    , ( EditorIndexCenter, EditorIndexBottom )
    , ( EditorIndexRight, EditorIndexTop )
    , ( EditorIndexRight, EditorIndexBottom )
    ]


{-| リサイズのためにつかむガター
-}
type Gutter
    = GutterVertical GutterVertical
    | GutterHorizontal GutterHorizontal


{-| 垂直向きで左右に動かすガター | |
-}
type GutterVertical
    = GutterVerticalLeft
    | GutterVerticalRight


{-| 水平向きで上下に動かすガター - - -
-}
type GutterHorizontal
    = GutterHorizontalLeft
    | GutterHorizontalCenter
    | GutterHorizontalRight


{-| EditorGroupへのメッセージ
-}
type Msg
    = ChangeActiveEditor EditorIndex -- 他のエディタへアクティブなエディタを変更する
    | OpenEditor OpenEditorPosition -- エディタを表示する
    | CloseEditor EditorIndex -- エディタを削除する
    | FireClickEventInCapturePhase String -- エディタをクリックしてアクティブにする
    | EditorItemMsg { msg : EditorMsg, ref : EditorIndex } -- 内包しているエディタへのMsg
    | EditorItemMsgToActive EditorMsg -- アクティブなエディタへのMsg
    | GrabVerticalGutter GutterVertical -- |垂直Gutterをつかむ
    | GrabHorizontalGutter GutterHorizontal -- -水平Gutterをつかむ
    | Focus -- フォーカスが当たる
    | Blur -- フォカスが外れる


type EditorMsg
    = EditorKeyConfigMsg Component.Editor.EditorKeyConfig.Msg
    | ModuleEditorMsg Component.Editor.Module.Msg


{-| 開くエディタの位置
-}
type OpenEditorPosition
    = OpenEditorPositionRightRow
    | OpenEditorPositionLeftBottom
    | OpenEditorPositionCenterBottom
    | OpenEditorPositionRightBottom


{-| EditorGroupから発生するコマンド
-}
type Cmd
    = CmdVerticalGutterModeOn GutterVertical
    | CmdHorizontalGutterModeOn GutterHorizontal
    | CmdSetTextAreaValue String
    | CmdFocusEditTextAea
    | CmdElementScrollIntoView String
    | CmdFocusHere
    | CmdNone


{-| 初期Model
-}
initModel : ( Model, List Cmd )
initModel =
    ( Model
        { group =
            RowOne
                { left =
                    ColumnOne
                        { top = ModuleEditor (Component.Editor.Module.initModel (Data.IdHash.ModuleId "")) }
                }
        , activeEditorIndex = ( EditorIndexLeft, EditorIndexTop )
        }
    , []
    )


{-| 開いていてかつ選択していてアクティブなエディタ(参照,種類)を取得する
-}
getActiveEditor : Model -> Component.EditorItemSource.EditorItemSource
getActiveEditor model =
    case getEditorItem (getActiveEditorRef model) (getGroup model) of
        ProjectEditor _ ->
            Component.EditorItemSource.ProjectRoot

        ConfigEditor _ ->
            Component.EditorItemSource.ProjectImport

        ModuleEditor editorModel ->
            Component.EditorItemSource.Module (Component.Editor.Module.getTargetModuleIndex editorModel)

        EditorKeyConfig _ ->
            Component.EditorItemSource.EditorKeyConfig


{-| テキストエリアにフォーカスが当たっているか。
当たっていたらKey.ArrowLeftなどのキー入力をpreventDefaultしない。ブラウザの基本機能(訂正など)を阻止しない
-}
isFocusDefaultUi : Model -> Maybe Component.DefaultUi.DefaultUi
isFocusDefaultUi model =
    case getEditorItem (getActiveEditorRef model) (getGroup model) of
        ModuleEditor moduleEditorModel ->
            Component.Editor.Module.isFocusDefaultUi moduleEditorModel

        _ ->
            Nothing



{- ====================== Update ====================== -}


update : Msg -> Data.Project.Project -> Model -> ( Model, List Cmd )
update msg project model =
    case msg of
        ChangeActiveEditor activeEditorIndex ->
            updateChangeActiveEditor activeEditorIndex project model

        OpenEditor openEditorIndex ->
            let
                ( newGroup, newActiveEditorIndex ) =
                    openEditor (getActiveEditorRef model) openEditorIndex (getGroup model)
            in
            ( model
                |> setGroup newGroup
                |> setActiveEditorRef newActiveEditorIndex
            , []
            )

        CloseEditor hideEditorRef ->
            ( model
                |> mapGroup (closeEditor hideEditorRef)
                |> normalizeActiveEditorRef
            , []
            )

        FireClickEventInCapturePhase idString ->
            ( fireClickEventInCapturePhase idString model
            , []
            )

        GrabHorizontalGutter gutter ->
            ( model
            , [ CmdHorizontalGutterModeOn gutter ]
            )

        GrabVerticalGutter gutter ->
            ( model
            , [ CmdVerticalGutterModeOn gutter ]
            )

        EditorItemMsg rec ->
            let
                ( newEditorItem, cmdList ) =
                    model
                        |> getGroup
                        |> getEditorItem rec.ref
                        |> updateEditor rec.msg project
            in
            ( model
                |> mapGroup (setEditorItem rec.ref newEditorItem)
            , cmdList
            )

        EditorItemMsgToActive editorItemMsg ->
            let
                ( newEditorItem, cmdList ) =
                    model
                        |> getGroup
                        |> getEditorItem (getActiveEditorRef model)
                        |> updateEditor editorItemMsg project
            in
            ( model
                |> mapGroup (setEditorItem (getActiveEditorRef model) newEditorItem)
            , cmdList
            )

        Focus ->
            let
                ( newEditorItem, cmdList ) =
                    model
                        |> getGroup
                        |> getEditorItem (getActiveEditorRef model)
                        |> focusEditor project
            in
            ( model
                |> mapGroup (setEditorItem (getActiveEditorRef model) newEditorItem)
            , cmdList ++ [ CmdFocusHere ]
            )

        Blur ->
            let
                ( newEditorItem, cmdList ) =
                    model
                        |> getGroup
                        |> getEditorItem (getActiveEditorRef model)
                        |> blurEditor project
            in
            ( model
                |> mapGroup (setEditorItem (getActiveEditorRef model) newEditorItem)
            , cmdList
            )


updateChangeActiveEditor : EditorIndex -> Data.Project.Project -> Model -> ( Model, List Cmd )
updateChangeActiveEditor index project model =
    let
        ( beforeActiveEditorNewModel, beforeActiveCmd ) =
            model
                |> getGroup
                |> getEditorItem (getActiveEditorRef model)
                |> blurEditor project

        newModel =
            model
                |> mapGroup (setEditorItem (getActiveEditorRef model) beforeActiveEditorNewModel)
                |> setActiveEditorRef index

        ( newEditorItem, cmd ) =
            newModel
                |> getGroup
                |> getEditorItem index
                |> focusEditor project
    in
    ( newModel
        |> mapGroup (setEditorItem index newEditorItem)
    , beforeActiveCmd ++ cmd
    )


{-| エディタにフォーカスが当たったことを知らせる
-}
focusEditor : Data.Project.Project -> EditorModel -> ( EditorModel, List Cmd )
focusEditor project editorItem =
    case editorItem of
        ModuleEditor model ->
            let
                ( newModel, cmdList ) =
                    Component.Editor.Module.update
                        Component.Editor.Module.MsgFocusThisEditor
                        project
                        model
            in
            ( ModuleEditor newModel
            , cmdList |> List.map moduleEditorCmdToCmd
            )

        _ ->
            ( editorItem, [] )


{-| エディタにフォーカスが外れたことを知らせる
-}
blurEditor : Data.Project.Project -> EditorModel -> ( EditorModel, List Cmd )
blurEditor project editorItem =
    case editorItem of
        ModuleEditor model ->
            let
                ( newModel, cmdList ) =
                    Component.Editor.Module.update
                        Component.Editor.Module.MsgBlurThisEditor
                        project
                        model
            in
            ( ModuleEditor newModel
            , cmdList |> List.map moduleEditorCmdToCmd
            )

        _ ->
            ( editorItem, [] )


updateEditor : EditorMsg -> Data.Project.Project -> EditorModel -> ( EditorModel, List Cmd )
updateEditor editorItemMsg project editorItem =
    case ( editorItemMsg, editorItem ) of
        ( ModuleEditorMsg msg, ModuleEditor model ) ->
            let
                ( newModel, cmdList ) =
                    Component.Editor.Module.update msg project model
            in
            ( ModuleEditor newModel
            , cmdList |> List.map moduleEditorCmdToCmd
            )

        ( EditorKeyConfigMsg msg, EditorKeyConfig model ) ->
            let
                ( newModel, _ ) =
                    Component.Editor.EditorKeyConfig.update msg model
            in
            ( EditorKeyConfig newModel
            , []
            )

        _ ->
            ( editorItem
            , []
            )


moduleEditorCmdToCmd : Component.Editor.Module.Cmd -> Cmd
moduleEditorCmdToCmd cmd =
    case cmd of
        Component.Editor.Module.CmdSetTextAreaValue text ->
            CmdSetTextAreaValue text

        Component.Editor.Module.CmdFocusEditTextAea ->
            CmdFocusEditTextAea

        Component.Editor.Module.CmdElementScrollIntoView id ->
            CmdElementScrollIntoView id

        Component.Editor.Module.None ->
            CmdNone


{-| 右端と下の端にある表示するエディタを増やすのボタンをおしたら、エディタ全体がどう変わるかと新しくアクティブになるエディタを返す
-}
openEditor : EditorIndex -> OpenEditorPosition -> Group -> ( Group, EditorIndex )
openEditor activeEditorIndex showEditorPosition group =
    (case group of
        RowOne { left } ->
            openEditorRowOne
                left
                showEditorPosition
                (getEditorItem activeEditorIndex group)

        RowTwo rec ->
            openEditorRowTwo
                rec
                showEditorPosition
                (getEditorItem activeEditorIndex group)

        RowThree rec ->
            openEditorRowThree
                rec
                showEditorPosition
                (getEditorItem activeEditorIndex group)
    )
        |> Maybe.withDefault ( group, activeEditorIndex )


openEditorRowOne : ColumnGroup -> OpenEditorPosition -> EditorModel -> Maybe ( Group, EditorIndex )
openEditorRowOne column addEditorPosition item =
    case addEditorPosition of
        OpenEditorPositionRightRow ->
            Just
                ( RowTwo
                    { left = column
                    , center = ColumnOne { top = item }
                    , leftWidth = 500
                    }
                , ( EditorIndexCenter, EditorIndexTop )
                )

        OpenEditorPositionLeftBottom ->
            case column of
                ColumnOne { top } ->
                    Just
                        ( RowOne
                            { left =
                                ColumnTwo
                                    { top = top
                                    , bottom = item
                                    , topHeight = 500
                                    }
                            }
                        , ( EditorIndexLeft, EditorIndexBottom )
                        )

                ColumnTwo _ ->
                    Nothing

        _ ->
            Nothing


openEditorRowTwo :
    { left : ColumnGroup
    , center : ColumnGroup
    , leftWidth : Int
    }
    -> OpenEditorPosition
    -> EditorModel
    -> Maybe ( Group, EditorIndex )
openEditorRowTwo rec addEditorPosition item =
    case addEditorPosition of
        OpenEditorPositionRightRow ->
            Just
                ( RowThree
                    { left = rec.left
                    , center = rec.center
                    , right = ColumnOne { top = item }
                    , leftWidth = 333
                    , centerWidth = 333
                    }
                , ( EditorIndexRight, EditorIndexTop )
                )

        OpenEditorPositionLeftBottom ->
            case rec.left of
                ColumnOne { top } ->
                    Just
                        ( RowTwo
                            { rec
                                | left =
                                    ColumnTwo
                                        { top = top
                                        , bottom = item
                                        , topHeight = 500
                                        }
                            }
                        , ( EditorIndexLeft, EditorIndexBottom )
                        )

                ColumnTwo _ ->
                    Nothing

        OpenEditorPositionCenterBottom ->
            case rec.center of
                ColumnOne { top } ->
                    Just
                        ( RowTwo
                            { rec
                                | center =
                                    ColumnTwo
                                        { top = top
                                        , bottom = item
                                        , topHeight = 500
                                        }
                            }
                        , ( EditorIndexCenter, EditorIndexBottom )
                        )

                ColumnTwo _ ->
                    Nothing

        _ ->
            Nothing


openEditorRowThree :
    { left : ColumnGroup
    , center : ColumnGroup
    , right : ColumnGroup
    , leftWidth : Int
    , centerWidth : Int
    }
    -> OpenEditorPosition
    -> EditorModel
    -> Maybe ( Group, EditorIndex )
openEditorRowThree rec addEditorPosition item =
    case addEditorPosition of
        OpenEditorPositionLeftBottom ->
            case rec.left of
                ColumnOne { top } ->
                    Just
                        ( RowThree
                            { rec
                                | left =
                                    ColumnTwo
                                        { top = top
                                        , bottom = item
                                        , topHeight = 500
                                        }
                            }
                        , ( EditorIndexLeft, EditorIndexBottom )
                        )

                ColumnTwo _ ->
                    Nothing

        OpenEditorPositionCenterBottom ->
            case rec.center of
                ColumnOne { top } ->
                    Just
                        ( RowThree
                            { rec
                                | center =
                                    ColumnTwo
                                        { top = top
                                        , bottom = item
                                        , topHeight = 500
                                        }
                            }
                        , ( EditorIndexCenter, EditorIndexBottom )
                        )

                ColumnTwo _ ->
                    Nothing

        OpenEditorPositionRightBottom ->
            case rec.right of
                ColumnOne { top } ->
                    Just
                        ( RowThree
                            { rec
                                | right =
                                    ColumnTwo
                                        { top = top
                                        , bottom = item
                                        , topHeight = 500
                                        }
                            }
                        , ( EditorIndexRight, EditorIndexBottom )
                        )

                ColumnTwo _ ->
                    Nothing

        _ ->
            Nothing


{-| エディタを閉じる
-}
closeEditor : EditorIndex -> Group -> Group
closeEditor index group =
    case group of
        RowOne rec ->
            case index of
                ( EditorIndexLeft, editorRefColumn ) ->
                    closeEditorColumn editorRefColumn rec.left
                        |> Maybe.map (\col -> RowOne { rec | left = col })
                        |> Maybe.withDefault group

                _ ->
                    group

        RowTwo rec ->
            case Tuple.first index of
                EditorIndexLeft ->
                    closeEditorColumn (Tuple.second index) rec.left
                        |> Maybe.map (\col -> RowTwo { rec | left = col })
                        |> Maybe.withDefault
                            (RowOne { left = rec.center })

                EditorIndexCenter ->
                    closeEditorColumn (Tuple.second index) rec.center
                        |> Maybe.map (\col -> RowTwo { rec | center = col })
                        |> Maybe.withDefault
                            (RowOne { left = rec.left })

                _ ->
                    group

        RowThree rec ->
            case Tuple.first index of
                EditorIndexLeft ->
                    closeEditorColumn (Tuple.second index) rec.left
                        |> Maybe.map (\col -> RowThree { rec | left = col })
                        |> Maybe.withDefault
                            (RowTwo
                                { left = rec.center
                                , center = rec.right
                                , leftWidth = rec.centerWidth
                                }
                            )

                EditorIndexCenter ->
                    closeEditorColumn (Tuple.second index) rec.center
                        |> Maybe.map (\col -> RowThree { rec | center = col })
                        |> Maybe.withDefault
                            (RowTwo
                                { left = rec.left
                                , center = rec.right
                                , leftWidth = rec.leftWidth
                                }
                            )

                EditorIndexRight ->
                    closeEditorColumn (Tuple.second index) rec.right
                        |> Maybe.map (\col -> RowThree { rec | right = col })
                        |> Maybe.withDefault
                            (RowTwo
                                { left = rec.left
                                , center = rec.center
                                , leftWidth = rec.leftWidth
                                }
                            )


closeEditorColumn : EditorIndexColumn -> ColumnGroup -> Maybe ColumnGroup
closeEditorColumn editorRefColumn columnGroup =
    case ( editorRefColumn, columnGroup ) of
        ( _, ColumnOne _ ) ->
            Nothing

        ( EditorIndexTop, ColumnTwo { bottom } ) ->
            Just (ColumnOne { top = bottom })

        ( EditorIndexBottom, ColumnTwo { top } ) ->
            Just (ColumnOne { top = top })



{- ====================== Gutterでのリサイズ ====================== -}


{-| エディタで編集表示するものを変える
-}
changeActiveEditorResource : Component.EditorItemSource.EditorItemSource -> Model -> Model
changeActiveEditorResource projectRef model =
    changeEditorItem (projectRefToEditorItem projectRef) model


{-| ←|→ VerticalGutterでリサイズをする
-}
resizeFromVerticalGutter : { mouseRelX : Int, editorWidth : Int } -> GutterVertical -> Model -> Model
resizeFromVerticalGutter { mouseRelX, editorWidth } gutter model =
    model
        |> mapGroup
            (resizeVertical
                { x = mouseRelX
                , width = editorWidth
                }
                gutter
            )


{-| ↔ 左右方向のリサイズ
-}
resizeVertical : { x : Int, width : Int } -> GutterVertical -> Group -> Group
resizeVertical { x, width } gutter group =
    case group of
        RowOne _ ->
            group

        -- 横に分割していないのにリサイズしようとしている
        RowTwo rec ->
            case gutter of
                GutterVerticalLeft ->
                    RowTwo
                        { rec
                            | leftWidth =
                                clamp 100 900 (x * 1002 // width - 1)
                        }

                GutterVerticalRight ->
                    group

        -- 横に2しか分割していないのに右側のガターでリサイズしようとしている
        RowThree rec ->
            case gutter of
                GutterVerticalLeft ->
                    let
                        leftWidth =
                            clamp 100 800 (x * 1002 // width - 1)
                    in
                    RowThree
                        { rec
                            | leftWidth =
                                leftWidth
                            , centerWidth =
                                max 100 (rec.leftWidth + rec.centerWidth - leftWidth)
                        }

                GutterVerticalRight ->
                    let
                        leftWidth =
                            clamp 200 900 (x * 1002 // width - 1)
                    in
                    RowThree
                        { rec
                            | leftWidth =
                                if leftWidth - rec.leftWidth < 100 then
                                    leftWidth - 100

                                else
                                    rec.leftWidth
                            , centerWidth =
                                max 100 (leftWidth - rec.leftWidth)
                        }


{-| ↑/↓ HorizontalGutterでリサイズする
-}
resizeFromHorizontalGutter : { mouseRelY : Int, editorHeight : Int } -> GutterHorizontal -> Model -> Model
resizeFromHorizontalGutter { mouseRelY, editorHeight } gutter model =
    model
        |> mapGroup
            (resizeHorizontal
                { y = mouseRelY
                , height = editorHeight
                }
                gutter
            )


{-| ↕ 縦方向のリサイズ
-}
resizeHorizontal : { y : Int, height : Int } -> GutterHorizontal -> Group -> Group
resizeHorizontal { y, height } gutter group =
    case group of
        RowOne rec ->
            case gutter of
                GutterHorizontalLeft ->
                    resizeInColumn rec.left y height
                        |> Maybe.map (\col -> RowOne { rec | left = col })
                        |> Maybe.withDefault group

                _ ->
                    group

        RowTwo rec ->
            case gutter of
                GutterHorizontalLeft ->
                    resizeInColumn rec.left y height
                        |> Maybe.map (\col -> RowTwo { rec | left = col })
                        |> Maybe.withDefault group

                GutterHorizontalCenter ->
                    resizeInColumn rec.center y height
                        |> Maybe.map (\col -> RowTwo { rec | center = col })
                        |> Maybe.withDefault group

                _ ->
                    group

        RowThree rec ->
            case gutter of
                GutterHorizontalLeft ->
                    resizeInColumn rec.left y height
                        |> Maybe.map (\col -> RowThree { rec | left = col })
                        |> Maybe.withDefault group

                GutterHorizontalCenter ->
                    resizeInColumn rec.center y height
                        |> Maybe.map (\col -> RowThree { rec | center = col })
                        |> Maybe.withDefault group

                GutterHorizontalRight ->
                    resizeInColumn rec.right y height
                        |> Maybe.map (\col -> RowThree { rec | right = col })
                        |> Maybe.withDefault group


resizeInColumn : ColumnGroup -> Int -> Int -> Maybe ColumnGroup
resizeInColumn columnGroup mouseRelY editorHeight =
    case columnGroup of
        ColumnOne _ ->
            Nothing

        ColumnTwo rec ->
            Just
                (ColumnTwo
                    { rec
                        | topHeight = clamp 100 900 (mouseRelY * 1002 // editorHeight - 1)
                    }
                )



{- ======= エディタをアクティブにするクリック ======== -}


fireClickEventInCapturePhase : String -> Model -> Model
fireClickEventInCapturePhase idString model =
    case editorIndexFromIdString idString of
        Just editorIndex ->
            setActiveEditorRef editorIndex model

        Nothing ->
            model



{- ======= グループ(エディタの集まり) ======== -}


getGroup : Model -> Group
getGroup (Model { group }) =
    group


setGroup : Group -> Model -> Model
setGroup rowGroup (Model rec) =
    Model
        { rec
            | group = rowGroup
        }


mapGroup : (Group -> Group) -> Model -> Model
mapGroup =
    Utility.Map.toMapper getGroup setGroup



{- =========  アクティブなエディタ位置 ========== -}


getActiveEditorRef : Model -> EditorIndex
getActiveEditorRef (Model { activeEditorIndex }) =
    activeEditorIndex


setActiveEditorRefUnsafe : EditorIndex -> Model -> Model
setActiveEditorRefUnsafe activeEditorIndex (Model rec) =
    Model { rec | activeEditorIndex = activeEditorIndex }


{-| Activeなエディタを設定する。そのEditorRefが開かれていなければ、近くのものをActiveにする
-}
setActiveEditorRef : EditorIndex -> Model -> Model
setActiveEditorRef ( rowRef, colRef ) model =
    model
        |> setActiveEditorRefUnsafe
            (case getGroup model of
                RowOne { left } ->
                    ( EditorIndexLeft, adjustColumnRef left colRef )

                RowTwo { left, center } ->
                    case rowRef of
                        EditorIndexLeft ->
                            ( EditorIndexLeft, adjustColumnRef left colRef )

                        _ ->
                            ( EditorIndexCenter, adjustColumnRef center colRef )

                RowThree { left, center, right } ->
                    case rowRef of
                        EditorIndexLeft ->
                            ( EditorIndexLeft, adjustColumnRef left colRef )

                        EditorIndexCenter ->
                            ( EditorIndexCenter, adjustColumnRef center colRef )

                        EditorIndexRight ->
                            ( EditorIndexRight, adjustColumnRef right colRef )
            )


{-| editorColumnRefが存在するエディタを参照できるようにする
-}
adjustColumnRef : ColumnGroup -> EditorIndexColumn -> EditorIndexColumn
adjustColumnRef columnGroup editorRefColumn =
    case columnGroup of
        ColumnOne _ ->
            EditorIndexTop

        ColumnTwo _ ->
            editorRefColumn


mapActiveEditorRef : (EditorIndex -> EditorIndex) -> Model -> Model
mapActiveEditorRef =
    Utility.Map.toMapper getActiveEditorRef setActiveEditorRef


{-| アクティブなエディタが開かれていなければ、近くのものをActiveにする
-}
normalizeActiveEditorRef : Model -> Model
normalizeActiveEditorRef =
    mapActiveEditorRef identity


changeEditorItem : EditorModel -> Model -> Model
changeEditorItem item model =
    model
        |> mapGroup (setEditorItem (getActiveEditorRef model) item)


{-| エディタの位置を受け取って、エディタの中身(Modelとか)を返す
-}
getEditorItem : EditorIndex -> Group -> EditorModel
getEditorItem editorRef rowGroup =
    getEditorItemColumn (Tuple.second editorRef)
        (case rowGroup of
            RowOne { left } ->
                left

            RowTwo { left, center } ->
                case Tuple.first editorRef of
                    EditorIndexLeft ->
                        left

                    _ ->
                        center

            RowThree { left, center, right } ->
                case Tuple.first editorRef of
                    EditorIndexLeft ->
                        left

                    EditorIndexCenter ->
                        center

                    EditorIndexRight ->
                        right
        )


getEditorItemColumn : EditorIndexColumn -> ColumnGroup -> EditorModel
getEditorItemColumn editorRefCol colGroup =
    case colGroup of
        ColumnOne { top } ->
            top

        ColumnTwo { top, bottom } ->
            case editorRefCol of
                EditorIndexTop ->
                    top

                EditorIndexBottom ->
                    bottom


{-| エディタの中身を上書きする。指定するエディタの位置がないものだったらその左や上を上書きする
-}
setEditorItem : EditorIndex -> EditorModel -> Group -> Group
setEditorItem editorRef item group =
    case group of
        RowOne recRow ->
            RowOne
                { recRow
                    | left =
                        setEditorItemColumn (Tuple.second editorRef) item recRow.left
                }

        RowTwo recRow ->
            RowTwo
                (case Tuple.first editorRef of
                    EditorIndexLeft ->
                        { recRow
                            | left =
                                setEditorItemColumn (Tuple.second editorRef) item recRow.left
                        }

                    _ ->
                        { recRow
                            | center =
                                setEditorItemColumn (Tuple.second editorRef) item recRow.center
                        }
                )

        RowThree recRow ->
            RowThree
                (case Tuple.first editorRef of
                    EditorIndexLeft ->
                        { recRow
                            | left =
                                setEditorItemColumn (Tuple.second editorRef) item recRow.left
                        }

                    EditorIndexCenter ->
                        { recRow
                            | center =
                                setEditorItemColumn (Tuple.second editorRef) item recRow.center
                        }

                    EditorIndexRight ->
                        { recRow
                            | right =
                                setEditorItemColumn (Tuple.second editorRef) item recRow.right
                        }
                )


setEditorItemColumn : EditorIndexColumn -> EditorModel -> ColumnGroup -> ColumnGroup
setEditorItemColumn editorRefCol item columnGroup =
    case columnGroup of
        ColumnOne recCol ->
            ColumnOne { recCol | top = item }

        ColumnTwo recCol ->
            ColumnTwo
                (case editorRefCol of
                    EditorIndexTop ->
                        { recCol | top = item }

                    EditorIndexBottom ->
                        { recCol | bottom = item }
                )


{-| エディタの位置とエディタを加工する関数でGroupを更新する
-}
mapAtEditorItem : EditorIndex -> (EditorModel -> EditorModel) -> Group -> Group
mapAtEditorItem ref =
    Utility.Map.toMapper
        (getEditorItem ref)
        (setEditorItem ref)


{-| 編集対象からエディタの初期値を返す
-}
projectRefToEditorItem : Component.EditorItemSource.EditorItemSource -> EditorModel
projectRefToEditorItem projectRef =
    case projectRef of
        Component.EditorItemSource.ProjectRoot ->
            ProjectEditor Component.Editor.Project.initModel

        Component.EditorItemSource.ProjectImport ->
            ConfigEditor Component.Editor.ProjectImport.initModel

        Component.EditorItemSource.Module moduleRef ->
            ModuleEditor (Component.Editor.Module.initModel moduleRef)

        Component.EditorItemSource.EditorKeyConfig ->
            EditorKeyConfig Component.Editor.EditorKeyConfig.initModel



{- ====================== View ====================== -}


view :
    Data.Project.Project
    ->
        { width : Int
        , height : Int
        , language : Data.Language.Language
        , focus : Bool
        , gutter : Maybe Gutter
        }
    -> Model
    -> Html.Styled.Html Msg
view project { width, height, language, focus, gutter } (Model { group, activeEditorIndex }) =
    let
        ( activeEditorRow, activeEditorColumn ) =
            activeEditorIndex
    in
    Html.Styled.div
        ([ Html.Styled.Attributes.css
            [ Css.width (Css.px (toFloat width))
            , Css.height (Css.px (toFloat height))
            , Css.backgroundColor (Css.rgb 24 24 24)
            , Style.textColorStyle
            , Css.overflow Css.hidden
            , Css.displayFlex
            , Css.position Css.relative
            ]
         ]
            ++ (if focus then
                    []

                else
                    [ Html.Styled.Events.onClick Focus ]
               )
        )
        (case group of
            RowOne { left } ->
                [ editorColumn
                    project
                    left
                    { width = width - 2, height = height }
                    OpenEditorPositionLeftBottom
                    (Just activeEditorColumn)
                    EditorIndexLeft
                    (gutter == Just (GutterHorizontal GutterHorizontalLeft))
                    True
                ]

            RowTwo { left, center, leftWidth } ->
                [ editorColumn
                    project
                    left
                    { width = (width - 4) * leftWidth // 1000, height = height }
                    OpenEditorPositionLeftBottom
                    (case activeEditorRow of
                        EditorIndexLeft ->
                            Just activeEditorColumn

                        _ ->
                            Nothing
                    )
                    EditorIndexLeft
                    (gutter == Just (GutterHorizontal GutterHorizontalLeft))
                    False
                , verticalGutter
                    GutterVerticalLeft
                    (gutter == Just (GutterVertical GutterVerticalLeft))
                , editorColumn
                    project
                    center
                    { width = (width - 4) * (1000 - leftWidth) // 1000, height = height }
                    OpenEditorPositionCenterBottom
                    (case activeEditorRow of
                        EditorIndexLeft ->
                            Nothing

                        _ ->
                            Just activeEditorColumn
                    )
                    EditorIndexCenter
                    (gutter == Just (GutterHorizontal GutterHorizontalCenter))
                    False
                ]

            RowThree { left, center, right, leftWidth, centerWidth } ->
                [ editorColumn
                    project
                    left
                    { width = (width - 4) * leftWidth // 1000, height = height }
                    OpenEditorPositionLeftBottom
                    (case activeEditorRow of
                        EditorIndexLeft ->
                            Just activeEditorColumn

                        EditorIndexCenter ->
                            Nothing

                        EditorIndexRight ->
                            Nothing
                    )
                    EditorIndexLeft
                    (gutter == Just (GutterHorizontal GutterHorizontalLeft))
                    False
                , verticalGutter
                    GutterVerticalLeft
                    (gutter == Just (GutterVertical GutterVerticalLeft))
                , editorColumn
                    project
                    center
                    { width = (width - 4) * centerWidth // 1000, height = height }
                    OpenEditorPositionCenterBottom
                    (case activeEditorRow of
                        EditorIndexLeft ->
                            Nothing

                        EditorIndexCenter ->
                            Just activeEditorColumn

                        EditorIndexRight ->
                            Nothing
                    )
                    EditorIndexCenter
                    (gutter == Just (GutterHorizontal GutterHorizontalCenter))
                    False
                , verticalGutter
                    GutterVerticalRight
                    (gutter == Just (GutterVertical GutterVerticalRight))
                , editorColumn
                    project
                    right
                    { width = (width - 4) * (1000 - leftWidth - centerWidth) // 1000, height = height }
                    OpenEditorPositionRightBottom
                    (case activeEditorRow of
                        EditorIndexLeft ->
                            Nothing

                        EditorIndexCenter ->
                            Nothing

                        EditorIndexRight ->
                            Just activeEditorColumn
                    )
                    EditorIndexRight
                    (gutter == Just (GutterHorizontal GutterHorizontalRight))
                    False
                ]
        )


{-| | エディタの幅を変更するときにつかむガター
-}
verticalGutter : GutterVertical -> Bool -> Html.Styled.Html Msg
verticalGutter gutter isResizing =
    Style.verticalGutter isResizing
        |> Html.Styled.map (always (GrabVerticalGutter gutter))


{-| エディタの縦に2つ並んでいるか1つの表示
-}
editorColumn :
    Data.Project.Project
    -> ColumnGroup
    -> { width : Int, height : Int }
    -> OpenEditorPosition
    -> Maybe EditorIndexColumn
    -> EditorIndexRow
    -> Bool
    -> Bool
    -> Html.Styled.Html Msg
editorColumn project columnGroup { width, height } openEditorPosition activeEditorIndexColumnMaybe editorRefRow isGutterActive isOne =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.width (Css.px (toFloat width))
            , Css.displayFlex
            , Css.flexDirection Css.column
            ]
        ]
        (case columnGroup of
            ColumnOne { top } ->
                [ editorItemView
                    { project = project
                    , editorItem = top
                    , editorIndex = ( editorRefRow, EditorIndexTop )
                    , width = width
                    , height = height - 2
                    , isActive = Just EditorIndexTop == activeEditorIndexColumnMaybe
                    , isOne = isOne
                    }
                ]

            ColumnTwo { top, bottom, topHeight } ->
                [ editorItemView
                    { project = project
                    , editorItem = top
                    , editorIndex = ( editorRefRow, EditorIndexTop )
                    , width = width
                    , height = (height - 2) * topHeight // 1000
                    , isActive = Just EditorIndexTop == activeEditorIndexColumnMaybe
                    , isOne = False
                    }
                , horizontalGutter
                    (case editorRefRow of
                        EditorIndexLeft ->
                            GutterHorizontalLeft

                        EditorIndexCenter ->
                            GutterHorizontalCenter

                        EditorIndexRight ->
                            GutterHorizontalRight
                    )
                    isGutterActive
                , editorItemView
                    { project = project
                    , editorItem = bottom
                    , editorIndex = ( editorRefRow, EditorIndexBottom )
                    , width = width
                    , height = (height - 2) * (1000 - topHeight) // 1000
                    , isActive = Just EditorIndexBottom == activeEditorIndexColumnMaybe
                    , isOne = False
                    }
                ]
        )


{-| エディタの高さを変更するガター
-}
horizontalGutter : GutterHorizontal -> Bool -> Html.Styled.Html Msg
horizontalGutter gutter isResizing =
    Style.horizontalGutter isResizing
        |> Html.Styled.map (always (GrabHorizontalGutter gutter))


{-| それぞれのエディタの表示
-}
editorItemView :
    { project : Data.Project.Project
    , editorItem : EditorModel
    , editorIndex : EditorIndex
    , width : Int
    , height : Int
    , isActive : Bool
    , isOne : Bool
    }
    -> Html.Styled.Html Msg
editorItemView { project, editorItem, editorIndex, width, height, isActive, isOne } =
    let
        { title, body } =
            editorTitleAndBody width editorIndex isActive project editorItem
    in
    Html.Styled.div
        ([ Html.Styled.Attributes.css
            ([ Css.width (Css.px (toFloat width))
             , Css.height (Css.px (toFloat height))
             , Css.backgroundColor
                (if isActive then
                    Css.rgb 34 34 34

                 else
                    Css.rgb 24 24 24
                )
             , Css.displayFlex
             , Css.flexDirection Css.column
             , Css.overflow Css.auto
             ]
                ++ (if isActive then
                        [ Css.outline3 (Css.px 2) Css.solid (Css.rgb 255 165 0)
                        , Css.color (Css.rgb 170 170 170)
                        ]

                    else
                        [ Style.textColorStyle ]
                   )
            )
         , Html.Styled.Attributes.id (editorIndexToIdString editorIndex)
         ]
            ++ (if isActive then
                    []

                else
                    [ Html.Styled.Events.onClick (ChangeActiveEditor editorIndex) ]
               )
        )
        [ editorTitle title editorIndex isOne
        , Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.overflow Css.auto
                , Css.flexGrow (Css.int 1)
                ]
            ]
            body
        ]


editorIndexToIdString : EditorIndex -> String
editorIndexToIdString editorIndex =
    [ "editor"
    , case Tuple.first editorIndex of
        EditorIndexLeft ->
            "left"

        EditorIndexCenter ->
            "center"

        EditorIndexRight ->
            "right"
    , case Tuple.second editorIndex of
        EditorIndexTop ->
            "top"

        EditorIndexBottom ->
            "bottom"
    ]
        |> String.join "-"


editorIndexFromIdString : String -> Maybe EditorIndex
editorIndexFromIdString idString =
    Utility.ListExtra.getFirstSatisfyElement
        (\x -> editorIndexToIdString x == idString)
        editorIndexAllValue


editorTitleAndBody :
    Int
    -> EditorIndex
    -> Bool
    -> Data.Project.Project
    -> EditorModel
    -> { title : String, body : List (Html.Styled.Html Msg) }
editorTitleAndBody width editorIndex isActive project editorItem =
    case editorItem of
        ProjectEditor _ ->
            Component.Editor.Project.view

        ConfigEditor _ ->
            Component.Editor.ProjectImport.view

        ModuleEditor moduleEditorModel ->
            let
                viewItem =
                    Component.Editor.Module.view
                        { width = width
                        , project = project
                        , focus = isActive
                        }
                        moduleEditorModel
            in
            { title = viewItem.title
            , body =
                viewItem.body
                    |> List.map
                        (Html.Styled.map (\m -> EditorItemMsg { msg = ModuleEditorMsg m, ref = editorIndex }))
            }

        EditorKeyConfig model ->
            let
                viewItem =
                    Component.Editor.EditorKeyConfig.view model
            in
            { title = viewItem.title
            , body =
                viewItem.body
                    |> List.map
                        (Html.Styled.map
                            (\m ->
                                EditorItemMsg
                                    { msg = EditorKeyConfigMsg m, ref = editorIndex }
                            )
                        )
            }


{-| エディタのタイトル。closeableはパネルが1つのときにとじるボタンをなくすためにある
-}
editorTitle : String -> EditorIndex -> Bool -> Html.Styled.Html Msg
editorTitle title editorRef closeable =
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.flexShrink Css.zero
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.height (Css.px 40)
            , Css.boxShadow4 Css.zero (Css.px 2) (Css.px 4) (Css.rgba 0 0 0 0.4)
            ]
        ]
        ([ Html.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.flexGrow (Css.int 1)
                , Css.fontSize (Css.rem 1.5)
                , Css.padding2 Css.zero (Css.px 12)
                , Css.lineHeight (Css.num 1.2)
                , Css.letterSpacing (Css.px 0.15)
                ]
            ]
            [ Html.Styled.text title ]
         ]
            ++ (if closeable then
                    []

                else
                    [ editorTitleCloseIcon editorRef ]
               )
        )


{-| エディタを閉じるときに押すボタン
-}
editorTitleCloseIcon : EditorIndex -> Html.Styled.Html Msg
editorTitleCloseIcon editorRef =
    Html.Styled.button
        [ Html.Styled.Attributes.css
            [ Css.width (Css.px 40)
            , Css.height (Css.px 40)
            , Css.padding (Css.px 12)
            , Css.flexShrink Css.zero
            , Css.property "stroke" "rgb(221,221,221)"
            , Css.hover
                [ Css.property "stroke" "rgb(0,0,0)"
                , Css.backgroundColor (Css.rgb 255 0 0)
                ]
            , Css.active
                [ Css.property "stroke" "rgb(0,0,0)"
                , Css.backgroundColor (Css.rgb 244 67 54)
                ]
            ]
        , Html.Styled.Events.onClick (CloseEditor editorRef)
        ]
        [ VectorImage.toHtml
            { x = 0, y = 0, width = 12, height = 12 }
            Nothing
            [ VectorImage.line
                ( 1, 1 )
                ( 11, 11 )
                (VectorImage.strokeWidth 2)
            , VectorImage.line
                ( 11, 1 )
                ( 1, 11 )
                (VectorImage.strokeWidth 2)
            ]
        ]