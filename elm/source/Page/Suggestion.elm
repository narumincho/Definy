module Page.Suggestion exposing (Message, Model, getBrowserUiState, getSuggestionId, init, update, updateByCommonMessage, view)

import Array
import CommonUi
import Css
import Data
import Message
import Ui
import Utility


type Model
    = Loading Data.SuggestionId
    | Loaded LoadedModel
    | NotFound Data.SuggestionId


type LoadedModel
    = LoadedModel
        { id : Data.SuggestionId
        , snapshot : Data.SuggestionSnapshot
        , project : Maybe Data.ProjectSnapshot
        , select : Select
        , typePartList : List TypePart
        }


type Select
    = SelectTypePartArea
    | SelectTypePart Int
    | SelectTypePartName Int
    | SelectTypePartNameInput
        { index : Int
        , rawText : String
        }
    | SelectTypePartDescription Int
    | SelectTypePartBody Int
    | SelectPartArea


type TypePart
    = RequestName String
    | TypePart String


type Message
    = InputFromInputPanel String
    | RequestLogInUrl Data.OpenIdConnectProvider


init : Data.SuggestionId -> ( Model, Message.Command )
init suggestionId =
    ( Loading suggestionId
    , Message.GetSuggestion suggestionId
    )


getSuggestionId : Model -> Data.SuggestionId
getSuggestionId model =
    case model of
        Loading suggestionId ->
            suggestionId

        Loaded (LoadedModel { id }) ->
            id

        NotFound suggestionId ->
            suggestionId


getBrowserUiState : Model -> Message.BrowserUiState
getBrowserUiState model =
    case model of
        Loaded (LoadedModel { select }) ->
            case select of
                SelectTypePartArea ->
                    Message.NotFocus

                SelectTypePart _ ->
                    Message.NotFocus

                SelectTypePartName _ ->
                    Message.NotFocus

                SelectTypePartNameInput _ ->
                    Message.FocusInput

                SelectTypePartDescription _ ->
                    Message.NotFocus

                SelectTypePartBody _ ->
                    Message.NotFocus

                SelectPartArea ->
                    Message.NotFocus

        _ ->
            Message.NotFocus


updateByCommonMessage : Message.CommonMessage -> Model -> ( Model, Message.Command )
updateByCommonMessage commonMessage model =
    case ( commonMessage, model ) of
        ( Message.ResponseSuggestion suggestionResponse, _ ) ->
            if suggestionResponse.id == getSuggestionId model then
                case suggestionResponse.snapshotMaybe of
                    Just suggestionSnapshot ->
                        ( Loaded
                            (LoadedModel
                                { id = suggestionResponse.id
                                , snapshot = suggestionSnapshot
                                , project = Nothing
                                , select = SelectTypePartArea
                                , typePartList = []
                                }
                            )
                        , Message.Batch
                            [ Message.GetUser suggestionSnapshot.createUserId
                            , Message.GetProject suggestionSnapshot.projectId
                            ]
                        )

                    Nothing ->
                        ( NotFound suggestionResponse.id
                        , Message.None
                        )

            else
                ( model
                , Message.None
                )

        ( Message.ResponseProject projectSnapshot, _ ) ->
            case model of
                Loaded (LoadedModel loadedModel) ->
                    if loadedModel.snapshot.projectId == projectSnapshot.id then
                        case projectSnapshot.snapshotMaybe of
                            Just snapshot ->
                                ( Loaded
                                    (LoadedModel
                                        { loadedModel | project = Just snapshot }
                                    )
                                , Message.GetBlobUrl snapshot.imageHash
                                )

                            Nothing ->
                                ( model
                                , Message.None
                                )

                    else
                        ( model
                        , Message.None
                        )

                _ ->
                    ( model
                    , Message.None
                    )

        ( Message.SelectUp, Loaded (LoadedModel record) ) ->
            changeSelect (selectUp record.typePartList record.select) (LoadedModel record)

        ( Message.SelectDown, Loaded (LoadedModel record) ) ->
            changeSelect (selectDown record.typePartList record.select) (LoadedModel record)

        ( Message.SelectFirstChild, Loaded (LoadedModel record) ) ->
            changeSelect (selectFirstChild record.typePartList record.select) (LoadedModel record)

        ( Message.SelectParent, Loaded (LoadedModel record) ) ->
            changeSelect (selectParent record.select) (LoadedModel record)

        ( Message.NewElement, Loaded loadedModel ) ->
            newElement loadedModel |> Tuple.mapFirst Loaded

        _ ->
            ( model
            , Message.None
            )


changeSelect : Select -> LoadedModel -> ( Model, Message.Command )
changeSelect newSelect (LoadedModel record) =
    case record.select of
        SelectTypePartNameInput indexAndText ->
            ( Loaded
                (LoadedModel
                    { record
                        | select = newSelect
                        , typePartList =
                            Utility.setAt
                                indexAndText.index
                                (TypePart indexAndText.rawText)
                                record.typePartList
                    }
                )
            , Message.FocusElement (selectToFocusId newSelect)
            )

        _ ->
            ( Loaded
                (LoadedModel
                    { record
                        | select = newSelect
                    }
                )
            , Message.FocusElement (selectToFocusId newSelect)
            )


selectUp : List TypePart -> Select -> Select
selectUp typePartList select =
    case select of
        SelectTypePartArea ->
            SelectPartArea

        SelectTypePart int ->
            if int == 0 then
                SelectTypePartArea

            else
                SelectTypePart (int - 1)

        SelectTypePartDescription index ->
            SelectTypePartName index

        SelectTypePartBody index ->
            SelectTypePartDescription index

        SelectPartArea ->
            SelectTypePartArea

        _ ->
            selectParent select


selectDown : List TypePart -> Select -> Select
selectDown typePartList select =
    case select of
        SelectTypePartArea ->
            SelectPartArea

        SelectTypePart int ->
            if List.length typePartList - 1 <= int then
                selectParent select

            else
                SelectTypePart (int + 1)

        SelectTypePartName index ->
            SelectTypePartDescription index

        SelectTypePartDescription index ->
            SelectTypePartBody index

        SelectPartArea ->
            SelectTypePartArea

        _ ->
            selectParent select


selectFirstChild : List TypePart -> Select -> Select
selectFirstChild typePartList select =
    case select of
        SelectTypePartArea ->
            if List.length typePartList == 0 then
                SelectTypePartArea

            else
                SelectTypePart 0

        SelectTypePart int ->
            SelectTypePartName int

        SelectTypePartName int ->
            SelectTypePartNameInput
                { index = int, rawText = "" }

        SelectTypePartNameInput record ->
            SelectTypePartNameInput record

        SelectTypePartDescription index ->
            SelectTypePartDescription index

        SelectTypePartBody index ->
            SelectTypePartBody index

        SelectPartArea ->
            SelectPartArea


selectParent : Select -> Select
selectParent select =
    case select of
        SelectTypePartArea ->
            SelectTypePartArea

        SelectTypePart int ->
            SelectTypePartArea

        SelectTypePartName int ->
            SelectTypePart int

        SelectTypePartNameInput record ->
            SelectTypePartName record.index

        SelectTypePartDescription index ->
            SelectTypePart index

        SelectTypePartBody index ->
            SelectTypePart index

        SelectPartArea ->
            SelectPartArea


newElement : LoadedModel -> ( LoadedModel, Message.Command )
newElement (LoadedModel record) =
    case record.select of
        SelectTypePartArea ->
            let
                select =
                    SelectTypePartNameInput
                        { index = List.length record.typePartList
                        , rawText = ""
                        }
            in
            ( LoadedModel
                { record
                    | select = select
                    , typePartList = record.typePartList ++ [ TypePart "new" ]
                }
            , Message.FocusElement (selectToFocusId select)
            )

        SelectTypePartName index ->
            let
                select =
                    SelectTypePartNameInput
                        { index = index + 1
                        , rawText = ""
                        }
            in
            ( LoadedModel
                { record | select = select }
            , Message.FocusElement (selectToFocusId select)
            )

        _ ->
            ( LoadedModel record
            , Message.None
            )


update : Message -> Model -> ( Model, Message.Command )
update message model =
    case ( message, model ) of
        ( RequestLogInUrl provider, _ ) ->
            ( model
            , Message.RequestLogInUrl provider
            )

        ( InputFromInputPanel text, Loaded (LoadedModel record) ) ->
            ( Loaded
                (LoadedModel
                    { record
                        | select =
                            case record.select of
                                SelectTypePartNameInput indexAndText ->
                                    SelectTypePartNameInput
                                        { indexAndText | rawText = text }

                                _ ->
                                    record.select
                    }
                )
            , Message.None
            )

        ( _, _ ) ->
            ( model, Message.None )


view : Message.SubModel -> Model -> Ui.Panel Message
view subModel model =
    Ui.row
        Ui.stretch
        Ui.stretch
        []
        [ CommonUi.sidebarView subModel
            (case model of
                Loaded (LoadedModel loadedModel) ->
                    case loadedModel.project of
                        Just projectSnapshot ->
                            CommonUi.ProjectSuggestion
                                { snapshot = projectSnapshot
                                , id = loadedModel.snapshot.projectId
                                }
                                loadedModel.snapshot.ideaId

                        Nothing ->
                            CommonUi.None

                _ ->
                    CommonUi.None
            )
            |> Ui.map RequestLogInUrl
        , Ui.column
            Ui.stretch
            Ui.stretch
            []
            [ case model of
                Loading (Data.SuggestionId suggestionIdAsString) ->
                    CommonUi.normalText 16 ("Suggestionを読込中. id=" ++ suggestionIdAsString)

                Loaded loadedModel ->
                    mainView subModel loadedModel

                NotFound (Data.SuggestionId suggestionIdAsString) ->
                    CommonUi.normalText 16 ("Suggestionが見つからなかった id=" ++ suggestionIdAsString)
            ]
        ]


mainView : Message.SubModel -> LoadedModel -> Ui.Panel Message
mainView subModel (LoadedModel record) =
    Ui.column
        Ui.stretch
        Ui.stretch
        []
        [ Ui.scroll
            Ui.stretch
            Ui.stretch
            []
            (Ui.column
                Ui.stretch
                Ui.auto
                []
                [ CommonUi.table
                    [ ( "提案名", CommonUi.normalText 16 record.snapshot.name )
                    , ( "変更理由", CommonUi.normalText 16 record.snapshot.reason )
                    , ( "作成者", CommonUi.userView subModel record.snapshot.createUserId )
                    , ( "取得日時", CommonUi.timeView subModel record.snapshot.getTime )
                    ]
                , typePartAreaView subModel record.typePartList record.select
                , partAreaView subModel record.select
                ]
            )
        , inputPanel record.select
        ]


typePartAreaView : Message.SubModel -> List TypePart -> Select -> Ui.Panel Message
typePartAreaView subModel typeNameList select =
    Ui.column
        Ui.stretch
        Ui.auto
        [ Ui.focusAble (selectToFocusId SelectTypePartArea)
        , Ui.padding 7
        , elementBorderStyle (select == SelectTypePartArea)
        ]
        [ CommonUi.stretchText 16
            (case Message.getLanguage subModel of
                Data.LanguageEnglish ->
                    "TypePart"

                Data.LanguageJapanese ->
                    "型パーツ"

                Data.LanguageEsperanto ->
                    "Tajpu Parto"
            )
        , typePartListView typeNameList select
        ]


typePartListView : List TypePart -> Select -> Ui.Panel Message
typePartListView typePartList select =
    Ui.column
        Ui.stretch
        Ui.auto
        [ Ui.gap 4 ]
        (List.indexedMap (typePartView select) typePartList)


typePartView : Select -> Int -> TypePart -> Ui.Panel Message
typePartView select index typePart =
    Ui.column
        Ui.stretch
        (Ui.fix 70)
        [ Ui.focusAble (selectToFocusId (SelectTypePart index))
        , elementBackgroundStyle
        , Ui.padding 4
        , elementBorderStyle
            (case select of
                SelectTypePart int ->
                    int == index

                _ ->
                    False
            )
        ]
        [ typePartNameView select index typePart
        , typePartDescriptionView select index typePart
        , typePartBodyView select index typePart
        ]


typePartNameView : Select -> Int -> TypePart -> Ui.Panel Message
typePartNameView select index typePart =
    Ui.column
        Ui.stretch
        (Ui.fix 20)
        [ elementBorderStyle
            (case select of
                SelectTypePartName int ->
                    int == index

                _ ->
                    False
            )
        ]
        [ Ui.row
            Ui.stretch
            (Ui.fix 18)
            [ elementBorderStyle
                (case select of
                    SelectTypePartNameInput indexAndRowText ->
                        indexAndRowText.index == index

                    _ ->
                        False
                )
            ]
            [ CommonUi.normalText 16
                (case typePart of
                    TypePart name ->
                        name

                    RequestName name ->
                        name ++ "の変換を求めています"
                )
            , Ui.empty Ui.stretch Ui.auto []
            ]
        ]


typePartDescriptionView : Select -> Int -> TypePart -> Ui.Panel Message
typePartDescriptionView select index typePart =
    Ui.column
        Ui.stretch
        (Ui.fix 20)
        [ elementBorderStyle
            (case select of
                SelectTypePartDescription int ->
                    int == index

                _ ->
                    False
            )
        ]
        [ CommonUi.normalText 16 "型の説明" ]

typePartBodyView : Select -> Int -> TypePart -> Ui.Panel Message
typePartBodyView select index typePart =
    Ui.column
        Ui.stretch
        (Ui.fix 20)
        [ elementBorderStyle
            (case select of
                SelectTypePartBody int ->
                    int == index

                _ ->
                    False
            )
        ]
        [ CommonUi.normalText 16 "型の本体 直積か直和" ]


partAreaView : Message.SubModel -> Select -> Ui.Panel Message
partAreaView subModel select =
    Ui.column
        Ui.stretch
        Ui.auto
        [ Ui.padding 8
        , Ui.focusAble (selectToFocusId SelectPartArea)
        , elementBorderStyle (select == SelectPartArea)
        ]
        [ CommonUi.stretchText 24
            (case Message.getLanguage subModel of
                Data.LanguageEnglish ->
                    "Part"

                Data.LanguageJapanese ->
                    "パーツ"

                Data.LanguageEsperanto ->
                    "Parto"
            )
        , addPartView
        ]


addPartView : Ui.Panel Message
addPartView =
    Ui.column
        Ui.stretch
        Ui.auto
        [ elementBackgroundStyle
        , Ui.padding 4
        ]
        [ Ui.row
            Ui.stretch
            Ui.auto
            []
            [ Ui.empty
                (Ui.fix 256)
                (Ui.fix 32)
                [ partBorderStyle ]
            , CommonUi.normalText 24 ":"
            , Ui.empty
                (Ui.fix 256)
                (Ui.fix 32)
                [ partBorderStyle ]
            ]
        , Ui.row
            Ui.stretch
            Ui.auto
            []
            [ CommonUi.normalText 24 "="
            , Ui.empty
                (Ui.fix 512)
                (Ui.fix 32)
                [ partBorderStyle ]
            ]
        ]


partBorderStyle : Ui.Style
partBorderStyle =
    Ui.border
        (Ui.BorderStyle
            { color = Css.rgb 137 136 129
            , width =
                { top = 1
                , right = 1
                , left = 1
                , bottom = 1
                }
            }
        )


elementBorderStyle : Bool -> Ui.Style
elementBorderStyle isSelect =
    Ui.border
        (Ui.BorderStyle
            { color =
                if isSelect then
                    Css.rgb 27 227 2

                else
                    Css.rgba 0 0 0 0
            , width =
                { top = 1
                , right = 1
                , left = 1
                , bottom = 1
                }
            }
        )


elementBackgroundStyle : Ui.Style
elementBackgroundStyle =
    Ui.backgroundColor (Css.rgb 56 56 56)


inputPanelHeight : Int
inputPanelHeight =
    300


inputPanel : Select -> Ui.Panel Message
inputPanel select =
    Ui.column
        Ui.stretch
        (Ui.fix inputPanelHeight)
        [ Ui.border
            (Ui.BorderStyle
                { color = Css.rgb 200 200 200
                , width =
                    { top = 1
                    , right = 0
                    , left = 0
                    , bottom = 0
                    }
                }
            )
        ]
        (case select of
            SelectTypePartArea ->
                [ CommonUi.stretchText 16 "型パーツ全体を選択している" ]

            SelectTypePart index ->
                [ CommonUi.stretchText 16 (String.fromInt index ++ "番目の型パーツ") ]

            SelectTypePartName int ->
                [ CommonUi.stretchText 16 ("Eで" ++ String.fromInt int ++ "番目の型パーツの名前を変更") ]

            SelectTypePartNameInput _ ->
                [ candidatesView ]

            SelectTypePartDescription index ->
                [ CommonUi.stretchText 16 (String.fromInt index ++ "番目の型パーツの説明") ]

            SelectTypePartBody index ->
                [ CommonUi.stretchText 16 (String.fromInt index ++ "番目の型パーツの中身") ]

            SelectPartArea ->
                [ CommonUi.stretchText 16 "パーツ全体を選択している" ]
        )


candidatesView : Ui.Panel Message
candidatesView =
    Ui.column
        (Ui.fix 400)
        Ui.stretch
        []
        [ Ui.textInput
            Ui.stretch
            Ui.auto
            []
            (Ui.TextInputAttributes
                { inputMessage = InputFromInputPanel
                , name = "new-type-name"
                , id = inputId
                , multiLine = False
                , fontSize = 24
                }
            )
        , CommonUi.normalText 16 "候補をここに表示する"
        ]


inputId : String
inputId =
    "input"


selectToFocusId : Select -> String
selectToFocusId select =
    case select of
        SelectTypePartArea ->
            "type-part-area"

        SelectTypePart int ->
            "type-part-" ++ String.fromInt int

        SelectTypePartName int ->
            "type-part-" ++ String.fromInt int ++ "-name"

        SelectTypePartNameInput _ ->
            inputId

        SelectTypePartDescription index ->
            "type-part-" ++ String.fromInt index ++ "-description"

        SelectTypePartBody index ->
            "type-part-" ++ String.fromInt index ++ "-body"

        SelectPartArea ->
            "part-area"
