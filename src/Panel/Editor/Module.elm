module Panel.Editor.Module exposing
    ( Emit(..)
    , Model
    , Msg(..)
    , getModuleRef
    , initModel
    , isFocusDefaultUi
    , update
    , view
    )

import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import NSvg
import Palette.X11
import Panel.DefaultUi
import Parser
import Parser.SimpleChar
import Project
import Project.Label as L
import Project.Source
import Project.Source.Module.Def as Def
import Project.Source.Module.Def.Expr as Expr
import Project.Source.Module.Def.Expr.Operator as Op
import Project.Source.Module.Def.Expr.Term as Term
import Project.Source.Module.Def.Name as Name
import Project.Source.Module.Def.Type as Type
import Project.Source.ModuleWithCache as ModuleWithCache
import Utility.ListExtra


type Model
    = Model
        { moduleRef : Project.Source.ModuleRef
        , active : Active
        }


type Msg
    = ActiveTo Active
    | SelectLeft
    | SelectRight
    | SelectUp
    | SelectDown
    | SelectFirstChild
    | SelectLastChild
    | SelectParent
    | SuggestNextOrSelectDown
    | SuggestPrevOrSelectUp
    | Input String
    | ToEditMode
    | ConfirmMultiLineTextField
    | AddPartDef
    | FocusThisEditor
    | BlurThisEditor


type Emit
    = EmitChangeReadMe { text : String, ref : Project.Source.ModuleRef }
    | EmitChangeName { name : Name.Name, index : Int, ref : Project.Source.ModuleRef }
    | EmitChangeType { type_ : Type.Type, index : Int, ref : Project.Source.ModuleRef }
    | EmitChangeExpr { expr : Expr.Expr, index : Int, ref : Project.Source.ModuleRef }
    | EmitAddPartDef { ref : Project.Source.ModuleRef }
    | EmitSetTextAreaValue String
    | EmitFocusEditTextAea


type Active
    = ActiveNone
    | ActiveDescription DescriptionActive
    | ActivePartDefList PartDefListActive


type DescriptionActive
    = ActiveDescriptionSelf
    | ActiveDescriptionText


type PartDefListActive
    = ActivePartDefListSelf
    | ActivePartDef ( Int, PartDefActive )


type PartDefActive
    = ActivePartDefSelf
    | ActivePartDefName (Maybe ( List ( Char, Bool ), Int ))
    | ActivePartDefType (Maybe ( List ( Char, Bool ), Int ))
    | ActivePartDefExpr PartDefExprActive


{-| 式の中で選択している位置。式の長さを超えるところを指定しているならば、それは式の末尾を表す
-}
type PartDefExprActive
    = ActivePartDefExprSelf
    | ActiveExprHead --     |abc + def + 28
    | ActiveExprTerm Int (Maybe (List ( Char, Bool ))) -- [abc]+ def + 28  Intの範囲は0..255
    | ActiveExprOp Int (Maybe (List ( Char, Bool ))) --  abc[+]def + 28  Intの範囲は0..254


{-| テキストエリアにフォーカスが当たっているか。
当たっていたらKey.ArrowLeftなどのキー入力をpreventDefaultしない。ブラウザの基本機能(訂正など)を阻止しない
-}
isFocusDefaultUi : Model -> Maybe Panel.DefaultUi.DefaultUi
isFocusDefaultUi (Model { active }) =
    case active of
        ActiveDescription ActiveDescriptionText ->
            Just Panel.DefaultUi.MultiLineTextField

        ActivePartDefList (ActivePartDef ( _, ActivePartDefName (Just _) )) ->
            Just Panel.DefaultUi.SingleLineTextField

        ActivePartDefList (ActivePartDef ( _, ActivePartDefType (Just _) )) ->
            Just Panel.DefaultUi.SingleLineTextField

        _ ->
            Nothing


initModel : Project.Source.ModuleRef -> Model
initModel moduleRef =
    Model
        { moduleRef = moduleRef
        , active = ActiveNone
        }


getModuleRef : Model -> Project.Source.ModuleRef
getModuleRef (Model { moduleRef }) =
    moduleRef


update : Msg -> Project.Project -> Model -> ( Model, List Emit )
update msg project (Model rec) =
    let
        targetModule =
            project
                |> Project.getSource
                |> Project.Source.getModule rec.moduleRef
    in
    case msg of
        ActiveTo active ->
            activeTo active (Model rec)

        SelectLeft ->
            update (ActiveTo (selectLeft targetModule rec.active)) project (Model rec)

        SelectRight ->
            update (ActiveTo (selectRight targetModule rec.active)) project (Model rec)

        SelectUp ->
            update (ActiveTo (selectUp targetModule rec.active)) project (Model rec)

        SelectDown ->
            update (ActiveTo (selectDown targetModule rec.active)) project (Model rec)

        SelectFirstChild ->
            update (ActiveTo (selectFirstChild targetModule rec.active)) project (Model rec)

        SelectLastChild ->
            update (ActiveTo (selectLastChild targetModule rec.active)) project (Model rec)

        SelectParent ->
            update (ActiveTo (selectParent targetModule rec.active)) project (Model rec)

        SuggestPrevOrSelectUp ->
            case rec.active of
                ActivePartDefList (ActivePartDef ( index, ActivePartDefName (Just ( textAreaValue, suggestIndex )) )) ->
                    ( Model
                        { rec
                            | active =
                                ActivePartDefList
                                    (ActivePartDef
                                        ( index, ActivePartDefName (Just ( textAreaValue, max 0 (suggestIndex - 1) )) )
                                    )
                        }
                    , []
                    )

                _ ->
                    update SelectUp project (Model rec)

        SuggestNextOrSelectDown ->
            case rec.active of
                ActivePartDefList (ActivePartDef ( index, ActivePartDefName (Just ( textAreaValue, suggestIndex )) )) ->
                    ( Model
                        { rec
                            | active =
                                ActivePartDefList
                                    (ActivePartDef
                                        ( index, ActivePartDefName (Just ( textAreaValue, suggestIndex + 1 )) )
                                    )
                        }
                    , []
                    )

                _ ->
                    update SelectUp project (Model rec)

        Input string ->
            input string (Model rec)

        ToEditMode ->
            ( Model rec
            , []
            )

        ConfirmMultiLineTextField ->
            update (ActiveTo (confirmMultiLineTextField rec.active)) project (Model rec)

        AddPartDef ->
            ( Model rec
            , [ EmitAddPartDef { ref = rec.moduleRef } ]
            )

        FocusThisEditor ->
            ( Model rec
            , []
            )

        BlurThisEditor ->
            ( Model
                { rec
                    | active =
                        case rec.active of
                            ActiveDescription ActiveDescriptionText ->
                                ActiveDescription ActiveDescriptionSelf

                            _ ->
                                rec.active
                }
            , []
            )


activeTo : Active -> Model -> ( Model, List Emit )
activeTo active (Model rec) =
    ( Model { rec | active = active }
    , case active of
        ActiveNone ->
            []

        ActiveDescription ActiveDescriptionSelf ->
            []

        ActiveDescription ActiveDescriptionText ->
            [ EmitFocusEditTextAea ]

        ActivePartDefList ActivePartDefListSelf ->
            [ EmitFocusEditTextAea ]

        ActivePartDefList (ActivePartDef ( _, ActivePartDefSelf )) ->
            [ EmitFocusEditTextAea ]

        ActivePartDefList (ActivePartDef ( _, ActivePartDefName Nothing )) ->
            [ EmitFocusEditTextAea, EmitSetTextAreaValue "" ]

        ActivePartDefList (ActivePartDef ( _, ActivePartDefType Nothing )) ->
            [ EmitFocusEditTextAea, EmitSetTextAreaValue "" ]

        ActivePartDefList (ActivePartDef ( _, ActivePartDefExpr ActivePartDefExprSelf )) ->
            [ EmitFocusEditTextAea, EmitSetTextAreaValue "" ]

        _ ->
            []
    )


{-| 選択を左へ移動して、選択する対象を変える
-}
selectLeft : ModuleWithCache.Module -> Active -> Active
selectLeft module_ active =
    case active of
        ActiveNone ->
            -- 何も選択していないところから定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActivePartDefList ActivePartDefListSelf ->
            -- 定義リストから概要欄へ
            ActiveDescription ActiveDescriptionSelf

        ActivePartDefList (ActivePartDef ( 0, ActivePartDefSelf )) ->
            -- 先頭の定義から定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf )) ->
            -- 定義から前の定義へ
            ActivePartDefList (ActivePartDef ( index - 1, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing )) ->
            -- 名前から定義へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing )) ->
            -- 型から名前へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf )) ->
            -- 式から名前へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActiveExprHead )) ->
            -- 先頭の項の前から式へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm 0 _) )) ->
            -- 先頭の項から先頭の項の前へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActiveExprHead ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprOp opIndex _) )) ->
            -- 演算子から前の項へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm opIndex Nothing) ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm termIndex _) )) ->
            -- 項から前の演算子へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprOp (termIndex - 1) Nothing) ))

        _ ->
            active


{-| 選択を右へ移動して、選択する対象を変える
-}
selectRight : ModuleWithCache.Module -> Active -> Active
selectRight module_ active =
    case active of
        ActiveNone ->
            -- 何も選択していないところから概要欄へ
            ActiveDescription ActiveDescriptionSelf

        ActiveDescription ActiveDescriptionSelf ->
            -- 概要欄から定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf )) ->
            -- 定義から次の定義へ
            ActivePartDefList (ActivePartDef ( min (ModuleWithCache.getDefNum module_ - 1) (index + 1), ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing )) ->
            -- 名前から型へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing )) ->
            -- 型から式へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf )) ->
            -- 式から定義へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActiveExprHead )) ->
            -- 先頭の項の前から先頭の項へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm 0 Nothing) ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm termIndex _) )) ->
            -- 項から次の演算子へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprOp termIndex Nothing) ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprOp opIndex _) )) ->
            -- 演算子から次の項へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm (opIndex + 1) Nothing) ))

        _ ->
            active


{-| 選択を上へ移動して、選択する対象を変える
-}
selectUp : ModuleWithCache.Module -> Active -> Active
selectUp module_ active =
    case active of
        ActiveNone ->
            -- 何も選択していないところから定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActiveDescription _ ->
            -- 概要欄から概要欄へ
            ActiveDescription ActiveDescriptionSelf

        ActivePartDefList ActivePartDefListSelf ->
            -- 定義リストから概要欄へ
            ActiveDescription ActiveDescriptionSelf

        ActivePartDefList (ActivePartDef ( 0, ActivePartDefSelf )) ->
            -- 先頭の定義から定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf )) ->
            -- 定義から前の定義へ
            ActivePartDefList (ActivePartDef ( index - 1, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing )) ->
            -- 名前から定義へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing )) ->
            -- 型から定義へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf )) ->
            -- 式から名前へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr _ )) ->
            -- 式の中身から式へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        _ ->
            active


{-| 選択を下へ移動して、選択する対象を変える
-}
selectDown : ModuleWithCache.Module -> Active -> Active
selectDown module_ active =
    case active of
        ActiveNone ->
            -- 何も選択していないところから概要へ
            ActiveDescription ActiveDescriptionSelf

        ActiveDescription _ ->
            -- 概要欄から定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf )) ->
            -- 定義から次の定義へ
            ActivePartDefList (ActivePartDef ( min (ModuleWithCache.getDefNum module_ - 1) (index + 1), ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing )) ->
            -- 名前から式へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing )) ->
            -- 型から式へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf )) ->
            -- 式から定義へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr _ )) ->
            -- 式の中身から式へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        _ ->
            active


{-| 選択を選択していたものからその子供の先頭へ移動する
-}
selectFirstChild : ModuleWithCache.Module -> Active -> Active
selectFirstChild module_ active =
    case active of
        ActiveNone ->
            -- 何も選択していないところから概要へ
            ActiveDescription ActiveDescriptionSelf

        ActiveDescription ActiveDescriptionSelf ->
            -- 概要欄から概要欄のテキスト入力へ
            ActiveDescription ActiveDescriptionText

        ActivePartDefList ActivePartDefListSelf ->
            -- 定義リストから先頭の定義へ
            ActivePartDefList (ActivePartDef ( 0, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf )) ->
            -- 定義から名前へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf )) ->
            -- 式から先頭の項へ
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr (ActiveExprTerm 0 Nothing) ))

        _ ->
            active


{-| 選択を選択していたものからその子供の末尾へ移動する
-}
selectLastChild : ModuleWithCache.Module -> Active -> Active
selectLastChild module_ active =
    case active of
        ActiveNone ->
            -- 何も選択していないところから定義リストへ
            ActivePartDefList ActivePartDefListSelf

        ActiveDescription ActiveDescriptionSelf ->
            -- 概要欄から概要欄のテキスト入力へ
            ActiveDescription ActiveDescriptionText

        ActivePartDefList ActivePartDefListSelf ->
            -- 定義リストから最後の定義リストへ
            ActivePartDefList
                (ActivePartDef ( ModuleWithCache.getDefNum module_ - 1, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf )) ->
            -- 定義から式へ
            ActivePartDefList
                (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        _ ->
            active


selectParent : ModuleWithCache.Module -> Active -> Active
selectParent module_ active =
    case active of
        ActiveDescription ActiveDescriptionText ->
            ActiveDescription ActiveDescriptionSelf

        ActivePartDefList (ActivePartDef ( _, ActivePartDefSelf )) ->
            ActivePartDefList ActivePartDefListSelf

        ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing )) ->
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefName (Just _) )) ->
            ActivePartDefList (ActivePartDef ( index, ActivePartDefName Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing )) ->
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefType (Just _) )) ->
            ActivePartDefList (ActivePartDef ( index, ActivePartDefType Nothing ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf )) ->
            ActivePartDefList (ActivePartDef ( index, ActivePartDefSelf ))

        ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr _ )) ->
            ActivePartDefList (ActivePartDef ( index, ActivePartDefExpr ActivePartDefExprSelf ))

        _ ->
            active


{-| 複数行入力の確定。概要や文字列リテラルでの入力を確定にする
-}
confirmMultiLineTextField : Active -> Active
confirmMultiLineTextField active =
    case active of
        ActiveDescription ActiveDescriptionText ->
            ActiveDescription ActiveDescriptionSelf

        _ ->
            active


input : String -> Model -> ( Model, List Emit )
input string (Model rec) =
    if String.isEmpty (String.trim string) then
        ( Model rec
        , []
        )

    else
        case rec.active of
            ActiveDescription ActiveDescriptionText ->
                ( Model rec
                , [ EmitChangeReadMe { text = string, ref = rec.moduleRef } ]
                )

            ActivePartDefList (ActivePartDef ( index, ActivePartDefName _ )) ->
                let
                    ( active, emitList ) =
                        parserBeginWithName string index rec.moduleRef
                in
                ( Model { rec | active = active }
                , emitList
                )

            ActivePartDefList (ActivePartDef ( index, ActivePartDefType _ )) ->
                let
                    { type_, textAreaValue } =
                        parserBeginWithType string
                in
                ( Model
                    { rec
                        | active = ActivePartDefList (ActivePartDef ( index, ActivePartDefType (Just ( textAreaValue, 0 )) ))
                    }
                , [ EmitChangeType { type_ = type_, index = index, ref = rec.moduleRef } ]
                )

            _ ->
                ( Model rec
                , []
                )


parserBeginWithName : String -> Int -> Project.Source.ModuleRef -> ( Active, List Emit )
parserBeginWithName string index moduleRef =
    case Parser.beginWithName (Parser.SimpleChar.fromString string) of
        Parser.BeginWithNameEndName { name, textAreaValue } ->
            ( ActivePartDefList (ActivePartDef ( index, ActivePartDefName (Just ( textAreaValue, 0 )) ))
            , [ EmitChangeName { name = name, index = index, ref = moduleRef } ]
            )

        Parser.BeginWithNameEndType { name, type_, textAreaValue } ->
            ( ActivePartDefList (ActivePartDef ( index, ActivePartDefType (Just ( textAreaValue, 0 )) ))
            , [ EmitChangeName { name = name, index = index, ref = moduleRef } ]
                ++ (if Type.isEmpty type_ then
                        []

                    else
                        [ EmitChangeType { type_ = type_, index = index, ref = moduleRef } ]
                   )
            )

        Parser.BeginWithNameEndExprTerm { name, type_, headTerm, opAndTermList, textAreaValue } ->
            ( ActivePartDefList
                (ActivePartDef
                    ( index
                    , ActivePartDefExpr
                        (ActiveExprTerm (List.length opAndTermList) (Just textAreaValue))
                    )
                )
            , [ EmitChangeName { name = name, index = index, ref = moduleRef }
              , EmitChangeType { type_ = type_, index = index, ref = moduleRef }
              , EmitChangeExpr { expr = Expr.make headTerm opAndTermList, index = index, ref = moduleRef }
              ]
            )

        Parser.BeginWithNameEndExprOp { name, type_, headTerm, opAndTermList, lastOp, textAreaValue } ->
            ( ActivePartDefList
                (ActivePartDef
                    ( index, ActivePartDefExpr (ActiveExprOp (List.length opAndTermList) (Just textAreaValue)) )
                )
            , [ EmitChangeName { name = name, index = index, ref = moduleRef }
              , EmitChangeType { type_ = type_, index = index, ref = moduleRef }
              , EmitChangeExpr { expr = Expr.make headTerm (opAndTermList ++ [ ( lastOp, Term.none ) ]), index = index, ref = moduleRef }
              ]
            )


parserBeginWithType : String -> { type_ : Type.Type, textAreaValue : List ( Char, Bool ) }
parserBeginWithType string =
    case Parser.beginWithType (Parser.SimpleChar.fromString string) of
        Parser.BeginWithTypeEndType { type_, textAreaValue } ->
            { type_ = type_
            , textAreaValue = textAreaValue
            }

        Parser.BeginWithTypeEndExprTerm { type_, textAreaValue } ->
            { type_ = type_
            , textAreaValue = textAreaValue
            }

        Parser.BeginWithTypeEndExprOp { type_, textAreaValue } ->
            { type_ = type_
            , textAreaValue = textAreaValue
            }



{- ================================================
   ==================================================
                       View
   ==================================================
   ================================================
-}


{-| モジュールエディタのview。
プロジェクト全体のデータと
このエディタが全体にとってフォーカスが当たっているか当たっていないかのBoolと
モジュールエディタのModelで見た目を決める
-}
view : Project.Project -> Bool -> Model -> { title : String, body : List (Html.Html Msg) }
view project isFocus (Model { moduleRef, active }) =
    let
        targetModule =
            project
                |> Project.getSource
                |> Project.Source.getModule moduleRef
    in
    { title = L.toCapitalString (ModuleWithCache.getName targetModule)
    , body =
        [ Html.div [] [ Html.text (activeToString active) ]
        , descriptionView (ModuleWithCache.getReadMe targetModule)
            isFocus
            (case active of
                ActiveDescription descriptionActive ->
                    Just descriptionActive

                _ ->
                    Nothing
            )
        , partDefinitionsView
            isFocus
            (case active of
                ActivePartDefList partDefListActive ->
                    Just partDefListActive

                _ ->
                    Nothing
            )
            (ModuleWithCache.getDefWithCacheList targetModule |> List.map Tuple.first)
        ]
    }


activeToString : Active -> String
activeToString active =
    case active of
        ActiveNone ->
            "アクティブなし"

        ActiveDescription ActiveDescriptionSelf ->
            "概要欄"

        ActiveDescription ActiveDescriptionText ->
            "概要欄のテキストを編集している"

        ActivePartDefList ActivePartDefListSelf ->
            "パーツエディタ全体"

        ActivePartDefList (ActivePartDef ( index, partDefActive )) ->
            String.fromInt index
                ++ "番目の定義"
                ++ (partDefActive |> partDefActiveToString)


partDefActiveToString : PartDefActive -> String
partDefActiveToString partDefActive =
    case partDefActive of
        ActivePartDefSelf ->
            "全体"

        ActivePartDefName Nothing ->
            "名前"

        ActivePartDefName (Just _) ->
            "名前を編集中"

        ActivePartDefType Nothing ->
            "型"

        ActivePartDefType (Just _) ->
            "型を編集中"

        ActivePartDefExpr ActivePartDefExprSelf ->
            "式全体"

        ActivePartDefExpr ActiveExprHead ->
            "先頭の項の前"

        ActivePartDefExpr (ActiveExprTerm index Nothing) ->
            String.fromInt index ++ "番目の項"

        ActivePartDefExpr (ActiveExprTerm index (Just _)) ->
            String.fromInt index ++ "番目の項を編集中"

        ActivePartDefExpr (ActiveExprOp index Nothing) ->
            String.fromInt index ++ "番目の演算子"

        ActivePartDefExpr (ActiveExprOp index (Just _)) ->
            String.fromInt index ++ "番目の演算子を編集中"



{- ===== descriptionView ===== -}


descriptionView : String -> Bool -> Maybe DescriptionActive -> Html.Html Msg
descriptionView description isFocus descriptionActiveMaybe =
    let
        editHere =
            case descriptionActiveMaybe of
                Just ActiveDescriptionText ->
                    isFocus

                _ ->
                    False
    in
    Html.div
        ([ subClassList
            [ ( "description", True )
            , ( "description-active", descriptionActiveMaybe == Just ActiveDescriptionSelf )
            ]
         ]
            ++ (case descriptionActiveMaybe of
                    Just ActiveDescriptionSelf ->
                        []

                    _ ->
                        [ Html.Events.onClick (ActiveTo (ActiveDescription ActiveDescriptionSelf)) ]
               )
        )
        [ descriptionViewTitle
        , descriptionViewInputArea description isFocus descriptionActiveMaybe
        ]


descriptionViewTitle : Html.Html Msg
descriptionViewTitle =
    Html.h2
        [ subClass "description-title" ]
        [ Html.text "Description" ]


descriptionViewInputArea : String -> Bool -> Maybe DescriptionActive -> Html.Html Msg
descriptionViewInputArea description isFocus descriptionActiveMaybe =
    Html.div [ subClass "description-inputArea" ]
        [ Html.div
            [ subClassList
                [ ( "description-container", True )
                , ( "description-container-active", descriptionActiveMaybe == Just ActiveDescriptionText )
                ]
            ]
            [ descriptionViewMeasure description
            , descriptionViewTextArea description isFocus descriptionActiveMaybe
            ]
        ]


descriptionViewMeasure : String -> Html.Html Msg
descriptionViewMeasure description =
    let
        lineList =
            description |> String.lines
    in
    Html.div
        [ subClass "description-measure" ]
        ((lineList
            |> List.map Html.text
            |> List.intersperse (Html.br [] [])
         )
            ++ (if Utility.ListExtra.last lineList == Just "" then
                    [ Html.div [] [ Html.text "_" ] ]

                else
                    []
               )
        )


descriptionViewTextArea : String -> Bool -> Maybe DescriptionActive -> Html.Html Msg
descriptionViewTextArea description isFocus descriptionActiveMaybe =
    Html.textarea
        ([ subClass "description-textarea"
         ]
            ++ (case descriptionActiveMaybe of
                    Just ActiveDescriptionSelf ->
                        [ Html.Attributes.property "value" (Json.Encode.string description)
                        ]
                            ++ (if isFocus then
                                    [ Html.Events.stopPropagationOn "click" focusEventJsonDecoder ]

                                else
                                    [ Html.Events.onClick (ActiveTo (ActiveDescription ActiveDescriptionText)) ]
                               )

                    Just ActiveDescriptionText ->
                        [ Html.Events.onInput Input
                        , Html.Attributes.property "value" (Json.Encode.string description)
                        , Html.Events.stopPropagationOn "click" focusEventJsonDecoder
                        , subClass "description-textarea-focus"
                        ]
                            ++ (if isFocus then
                                    [ Html.Attributes.id "edit" ]

                                else
                                    []
                               )

                    Nothing ->
                        if isFocus then
                            [ Html.Events.stopPropagationOn "click" focusEventJsonDecoder
                            , Html.Attributes.property "value" (Json.Encode.string description)
                            ]

                        else
                            [ Html.Events.onClick (ActiveTo (ActiveDescription ActiveDescriptionText))
                            , Html.Attributes.property "value" (Json.Encode.string description)
                            ]
               )
        )
        []


focusEventJsonDecoder : Json.Decode.Decoder ( Msg, Bool )
focusEventJsonDecoder =
    Json.Decode.succeed
        ( ActiveTo (ActiveDescription ActiveDescriptionText), True )



{- ===== part definitions ===== -}


{-| モジュールエディタのメインの要素であるパーツエディタを表示する
partDefActiveMaybeAndIndexがJustならこのエディタ
-}
partDefinitionsView : Bool -> Maybe PartDefListActive -> List Def.Def -> Html.Html Msg
partDefinitionsView isFocus partDefListActiveMaybe defList =
    Html.div
        ([ subClass "partDefinitions"
         ]
            ++ (case partDefListActiveMaybe of
                    Just ActivePartDefListSelf ->
                        [ subClass "partDefinitions-active" ]

                    _ ->
                        [ Html.Events.onClick (ActiveTo (ActivePartDefList ActivePartDefListSelf)) ]
               )
        )
        ([ partDefinitionsViewTitle
         , partDefListView defList
            (case partDefListActiveMaybe of
                Just (ActivePartDef partDefActiveWithIndex) ->
                    Just partDefActiveWithIndex

                _ ->
                    Nothing
            )
         ]
            ++ (case partDefListActiveMaybe of
                    Just _ ->
                        if isFocus then
                            [ Html.textarea
                                [ Html.Attributes.class "partDef-hideTextArea"
                                , Html.Attributes.id "edit"
                                , Html.Events.onInput Input
                                ]
                                []
                            ]

                        else
                            []

                    Nothing ->
                        []
               )
        )


partDefinitionsViewTitle : Html.Html Msg
partDefinitionsViewTitle =
    Html.div
        [ subClass "partDefinitions-title" ]
        [ Html.text "Part Definitions" ]


partDefListView : List Def.Def -> Maybe ( Int, PartDefActive ) -> Html.Html Msg
partDefListView defList partDefActiveWithIndexMaybe =
    Html.div
        [ subClass "partDefList"
        ]
        ((defList
            |> List.indexedMap
                (\index def ->
                    partDefView index
                        def
                        (case partDefActiveWithIndexMaybe of
                            Just ( i, partDefActive ) ->
                                if i == index then
                                    Just partDefActive

                                else
                                    Nothing

                            _ ->
                                Nothing
                        )
                        |> Html.map (\m -> ActiveTo (ActivePartDefList (ActivePartDef ( index, m ))))
                )
         )
            ++ [ addDefButton ]
        )


partDefView : Int -> Def.Def -> Maybe PartDefActive -> Html.Html PartDefActive
partDefView index def partDefActiveMaybe =
    Html.div
        [ subClassList
            [ ( "partDef", True )
            , ( "partDef-active", partDefActiveMaybe == Just ActivePartDefSelf )
            ]
        , Html.Events.stopPropagationOn "click"
            (Json.Decode.succeed
                ( ActivePartDefSelf
                , True
                )
            )
        ]
        [ partDefViewNameAndType (Def.getName def) (Def.getType def) partDefActiveMaybe
        , partDefViewExpr (Def.getExpr def)
            (case partDefActiveMaybe of
                Just (ActivePartDefExpr partDefExprActive) ->
                    Just partDefExprActive

                _ ->
                    Nothing
            )
        ]



{- ================= Name And Type ================= -}
{------------------ Name  ------------------}


partDefViewNameAndType : Name.Name -> Type.Type -> Maybe PartDefActive -> Html.Html PartDefActive
partDefViewNameAndType name type_ partDefActiveMaybe =
    Html.div
        [ subClass "partDef-nameAndType" ]
        [ partDefViewName name
            (case partDefActiveMaybe of
                Just (ActivePartDefName textAreaValueAndIndexMaybe) ->
                    Just textAreaValueAndIndexMaybe

                _ ->
                    Nothing
            )
        , Html.text ":"
        , partDefViewType type_
            (case partDefActiveMaybe of
                Just (ActivePartDefType textAreaValueAndIndexMaybe) ->
                    Just textAreaValueAndIndexMaybe

                _ ->
                    Nothing
            )
        ]


partDefViewName : Name.Name -> Maybe (Maybe ( List ( Char, Bool ), Int )) -> Html.Html PartDefActive
partDefViewName name textAreaValueAndIndexMaybeMaybe =
    case textAreaValueAndIndexMaybeMaybe of
        Just (Just ( textAreaValue, suggestIndex )) ->
            partDefNameEditView name textAreaValue suggestIndex

        Just Nothing ->
            partDefNameNormalView name True

        Nothing ->
            partDefNameNormalView name False


partDefNameNormalView : Name.Name -> Bool -> Html.Html PartDefActive
partDefNameNormalView name isActive =
    case Name.toString name of
        Just nameString ->
            Html.div
                ([ subClassList
                    [ ( "partDef-name", True )
                    , ( "partDef-element-active", isActive )
                    ]
                 ]
                    ++ (if isActive then
                            []

                        else
                            [ Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( ActivePartDefName Nothing, True )) ]
                       )
                )
                [ Html.text nameString ]

        Nothing ->
            Html.div
                ([ subClassList
                    [ ( "partDef-noName", True )
                    , ( "partDef-element-active", isActive )
                    ]
                 ]
                    ++ (if isActive then
                            []

                        else
                            [ Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( ActivePartDefName Nothing, True )) ]
                       )
                )
                [ Html.text "NO NAME" ]


partDefNameEditView : Name.Name -> List ( Char, Bool ) -> Int -> Html.Html PartDefActive
partDefNameEditView name textAreaValue suggestIndex =
    Html.div
        [ subClass "partDef-name-edit" ]
        (textAreaValueToListHtml textAreaValue
            ++ [ suggestionName name suggestIndex ]
        )


suggestionName : Name.Name -> Int -> Html.Html msg
suggestionName name index =
    Html.div
        [ subClass "partDef-name-edit-suggestion" ]
        ([ ( name, enterIcon )
         , ( Name.fromLabel (L.make L.hg [ L.oa, L.om, L.oe ]), Html.text "ゲーム" )
         , ( Name.fromLabel (L.make L.hh [ L.oe, L.or, L.oo ]), Html.text "主人公" )
         , ( Name.fromLabel (L.make L.hb [ L.oe, L.oa, L.ou, L.ot, L.oi, L.of_, L.ou, L.ol, L.oG, L.oi, L.or, L.ol ]), Html.text "美少女" )
         , ( Name.fromLabel (L.make L.hm [ L.oo, L.on, L.os, L.ot, L.oe, L.or ]), Html.text "モンスター" )
         , ( Name.fromLabel (L.make L.hw [ L.oo, L.or, L.ol, L.od ]), Html.text "世界" )
         ]
            |> List.indexedMap
                (\i ( n, subItem ) ->
                    suggestNameItem n subItem (i == index)
                )
        )


suggestNameItem : Name.Name -> Html.Html msg -> Bool -> Html.Html msg
suggestNameItem name subItem isSelect =
    Html.div
        [ subClassList
            [ ( "partDef-name-edit-suggestion-item", True )
            , ( "partDef-name-edit-suggestion-item-select", isSelect )
            ]
        ]
        [ Html.div
            [ subClass "partDef-name-edit-suggestion-item-text" ]
            [ Html.text (Name.toString name |> Maybe.withDefault "<NO NAME>") ]
        , subItem
        ]


enterIcon : Html.Html msg
enterIcon =
    NSvg.toHtmlWithClass
        "moduleEditor-partDef-name-edit-suggestion-keyIcon"
        { x = 0, y = 0, width = 38, height = 32 }
        [ NSvg.polygon [ ( 4, 4 ), ( 34, 4 ), ( 34, 28 ), ( 12, 28 ), ( 12, 16 ), ( 4, 16 ) ] (NSvg.strokeColor Palette.X11.white) NSvg.fillNone
        , NSvg.path "M30,8 V20 H16 L18,18 M16,20 L18,22" (NSvg.strokeColor Palette.X11.white) NSvg.fillNone
        ]



{------------------ Type  ------------------}


partDefViewType : Type.Type -> Maybe (Maybe ( List ( Char, Bool ), Int )) -> Html.Html PartDefActive
partDefViewType type_ textAreaValueAndIndexMaybeMaybe =
    case textAreaValueAndIndexMaybeMaybe of
        Just (Just ( textAreaValue, _ )) ->
            partDefTypeEditView type_ textAreaValue

        Just Nothing ->
            partDefTypeNormalView type_ True

        Nothing ->
            partDefTypeNormalView type_ False


partDefTypeNormalView : Type.Type -> Bool -> Html.Html PartDefActive
partDefTypeNormalView type_ isActive =
    case Type.toString type_ of
        Just nameString ->
            Html.div
                ([ subClassList
                    [ ( "partDef-type", True )
                    , ( "partDef-element-active", isActive )
                    ]
                 ]
                    ++ (if isActive then
                            []

                        else
                            [ Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( ActivePartDefType Nothing, True )) ]
                       )
                )
                [ Html.text nameString ]

        Nothing ->
            Html.div
                ([ subClassList
                    [ ( "partDef-noType", True )
                    , ( "partDef-element-active", isActive )
                    ]
                 ]
                    ++ (if isActive then
                            []

                        else
                            [ Html.Events.stopPropagationOn "click" (Json.Decode.succeed ( ActivePartDefType Nothing, True )) ]
                       )
                )
                [ Html.text "NO TYPE" ]


partDefTypeEditView : Type.Type -> List ( Char, Bool ) -> Html.Html PartDefActive
partDefTypeEditView type_ textAreaValue =
    Html.div
        [ subClass "partDef-type-edit" ]
        (textAreaValueToListHtml textAreaValue)



{- ================= Expr ================= -}


partDefViewExpr : Expr.Expr -> Maybe PartDefExprActive -> Html.Html PartDefActive
partDefViewExpr expr partDefExprActiveMaybe =
    Html.div
        ([ subClass "partDef-expr"
         ]
            ++ (case partDefExprActiveMaybe of
                    Just ActivePartDefExprSelf ->
                        [ subClass "partDef-element-active" ]

                    _ ->
                        [ Html.Events.stopPropagationOn "click"
                            (Json.Decode.succeed ( ActivePartDefExpr ActivePartDefExprSelf, True ))
                        ]
               )
        )
        ([ Html.text "=" ]
            ++ (case partDefExprActiveMaybe of
                    Just ActiveExprHead ->
                        [ activeHeadTermLeft ]

                    _ ->
                        []
               )
            ++ [ termViewOutput (Expr.getHead expr) (partDefExprActiveMaybe == Just (ActiveExprTerm 0 Nothing))
                    |> Html.map (always (ActivePartDefExpr (ActiveExprTerm 0 Nothing)))
               ]
            ++ (Expr.getOthers expr
                    |> List.indexedMap
                        (\index ( op, term ) ->
                            [ opViewOutput op (partDefExprActiveMaybe == Just (ActiveExprOp index Nothing))
                                |> Html.map (always (ActiveExprOp index Nothing))
                            , termViewOutput term (partDefExprActiveMaybe == Just (ActiveExprTerm (index + 1) Nothing))
                                |> Html.map (always (ActiveExprTerm (index + 1) Nothing))
                            ]
                        )
                    |> List.concat
                    |> List.map (Html.map ActivePartDefExpr)
               )
        )


{-| 編集していない項の表示
-}
termViewOutput : Term.Term -> Bool -> Html.Html ()
termViewOutput term isActive =
    Html.div
        [ Html.Events.onClick ()
        , subClassList
            [ ( "partDef-term", True )
            , ( "partDef-element-active", isActive )
            ]
        ]
        [ Html.text (Term.toString term) ]


opViewOutput : Op.Operator -> Bool -> Html.Html ()
opViewOutput op isActive =
    Html.div
        [ Html.Events.onClick ()
        , subClassList
            [ ( "partDef-op", True )
            , ( "partDef-element-active", isActive )
            ]
        ]
        [ Html.text (Op.toString op |> Maybe.withDefault "?") ]


{-| 編集の表示
-}
textAreaValueToListHtml : List ( Char, Bool ) -> List (Html.Html msg)
textAreaValueToListHtml =
    List.map
        (\( char, bool ) ->
            Html.div
                [ subClass
                    (if bool then
                        "partDef-okChar"

                     else
                        "partDef-errChar"
                    )
                ]
                [ Html.text (String.fromChar char) ]
        )


{-| 項の先頭を表す
-}
activeHeadTermLeft : Html.Html msg
activeHeadTermLeft =
    Html.div
        [ subClass "partDef-caretBox" ]
        [ Html.div
            [ subClass "partDef-caret" ]
            []
        ]


addDefButton : Html.Html Msg
addDefButton =
    Html.button
        [ Html.Events.onClick AddPartDef
        , subClass "partDef-addPartDef"
        ]
        [ Html.text "+ 新しいパーツの定義" ]


subClass : String -> Html.Attribute msg
subClass class =
    case class of
        "" ->
            Html.Attributes.class "moduleEditor"

        _ ->
            Html.Attributes.class ("moduleEditor-" ++ class)


subClassList : List ( String, Bool ) -> Html.Attribute msg
subClassList =
    List.map (Tuple.mapFirst ((++) "moduleEditor-"))
        >> Html.Attributes.classList
