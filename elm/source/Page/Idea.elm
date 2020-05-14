module Page.Idea exposing (Message, Model, getIdeaId, init, update, updateByCommonMessage, view)

import CommonUi
import Data
import Data.LogInState
import Data.TimeZoneAndName
import Message
import Ui


type Model
    = Loading Data.IdeaId
    | NotFound Data.IdeaId
    | Loaded LoadedModel
    | CreatingSuggestion Data.IdeaId


type alias LoadedModel =
    { id : Data.IdeaId
    , snapshot : Data.IdeaSnapshot
    , comment : Comment
    , fresh : Bool
    , projectSnapshot : Maybe Data.ProjectSnapshot
    }


type Comment
    = Inputting String
    | Sending


type Message
    = InputComment String
    | Comment
    | Suggestion
    | RequestLogInUrl Data.OpenIdConnectProvider


init : Data.IdeaId -> ( Model, Message.Command )
init ideaId =
    ( Loading ideaId
    , Message.GetIdea ideaId
    )


getIdeaId : Model -> Data.IdeaId
getIdeaId model =
    case model of
        Loading ideaId ->
            ideaId

        NotFound ideaId ->
            ideaId

        Loaded ideaSnapshotAndId ->
            ideaSnapshotAndId.id

        CreatingSuggestion id ->
            id


updateByCommonMessage : Message.SubModel -> Message.CommonMessage -> Model -> ( Model, Message.Command )
updateByCommonMessage subModel message model =
    case message of
        Message.ResponseIdea idea ->
            if idea.id == getIdeaId model then
                case idea.snapshotMaybe of
                    Just snapshot ->
                        if isFresh subModel snapshot then
                            ( Loaded
                                { id = idea.id
                                , snapshot = snapshot
                                , comment = Inputting ""
                                , fresh = True
                                , projectSnapshot = Nothing
                                }
                            , Message.Batch
                                (Message.GetUser snapshot.createUserId
                                    :: List.map Message.GetUser
                                        (List.map .createUserId snapshot.itemList)
                                    ++ [ Message.GetProject snapshot.projectId ]
                                )
                            )

                        else
                            ( Loaded
                                { id = idea.id
                                , snapshot = snapshot
                                , comment = Inputting ""
                                , fresh = False
                                , projectSnapshot = Nothing
                                }
                            , Message.GetIdeaNoCache idea.id
                            )

                    Nothing ->
                        ( NotFound idea.id
                        , Message.None
                        )

            else
                ( model
                , Message.None
                )

        Message.ResponseAddSuggestion suggestionSnapshotAndIdMaybe ->
            let
                ideaId =
                    getIdeaId model
            in
            case suggestionSnapshotAndIdMaybe of
                Just suggestionSnapshotAndId ->
                    ( Loading ideaId
                    , Message.PushUrl
                        (Message.urlDataSameLanguageClientMode (Data.LocationSuggestion suggestionSnapshotAndId.id) subModel)
                    )

                Nothing ->
                    ( Loading ideaId
                    , Message.GetIdea ideaId
                    )

        Message.ResponseProject projectResponse ->
            case model of
                Loaded loadedModel ->
                    if loadedModel.snapshot.projectId == projectResponse.id then
                        ( Loaded { loadedModel | projectSnapshot = projectResponse.snapshotMaybe }
                        , case projectResponse.snapshotMaybe of
                            Just projectSnapshot ->
                                Message.GetBlobUrl projectSnapshot.imageHash

                            Nothing ->
                                Message.None
                        )

                    else
                        ( Loaded loadedModel
                        , Message.None
                        )

                _ ->
                    ( model
                    , Message.None
                    )

        Message.UpdateTime ->
            case model of
                Loaded loadedModel ->
                    if isFresh subModel loadedModel.snapshot then
                        ( model
                        , Message.None
                        )

                    else
                        ( model
                        , Message.GetIdeaNoCache loadedModel.id
                        )

                _ ->
                    ( model
                    , Message.None
                    )

        _ ->
            ( model
            , Message.None
            )


isFresh : Message.SubModel -> Data.IdeaSnapshot -> Bool
isFresh subModel ideaSnapshot =
    Data.TimeZoneAndName.isFresh 5000 (Message.getNowTime subModel) ideaSnapshot.getTime


update : Message -> Model -> ( Model, Message.Command )
update message model =
    case ( message, model ) of
        ( RequestLogInUrl provider, _ ) ->
            ( model
            , Message.RequestLogInUrl provider
            )

        ( InputComment comment, Loaded loadedModel ) ->
            ( Loaded
                { loadedModel | comment = Inputting comment }
            , Message.None
            )

        ( Comment, Loaded loadedModel ) ->
            case loadedModel.comment of
                Sending ->
                    ( model
                    , Message.None
                    )

                Inputting comment ->
                    ( Loaded
                        { loadedModel | comment = Sending }
                    , Message.AddComment
                        { ideaId = loadedModel.id
                        , comment = comment
                        }
                    )

        ( Suggestion, _ ) ->
            ( CreatingSuggestion (getIdeaId model)
            , Message.AddSuggestion (getIdeaId model)
            )

        ( _, _ ) ->
            ( model
            , Message.None
            )


view : Message.SubModel -> Model -> Ui.Panel Message
view subModel model =
    Ui.row
        Ui.stretch
        Ui.stretch
        []
        [ CommonUi.sidebarView subModel
            (case model of
                Loaded loadedModel ->
                    case loadedModel.projectSnapshot of
                        Just projectSnapshot ->
                            CommonUi.ProjectIdea
                                { id = loadedModel.snapshot.projectId, snapshot = projectSnapshot }

                        _ ->
                            CommonUi.None

                _ ->
                    CommonUi.None
            )
            |> Ui.map RequestLogInUrl
        , Ui.column
            Ui.stretch
            Ui.auto
            []
            [ case model of
                Loading ideaId ->
                    loadingView ideaId

                NotFound ideaId ->
                    notFoundView ideaId

                Loaded ideaSnapshotAndId ->
                    mainView subModel ideaSnapshotAndId

                CreatingSuggestion _ ->
                    CommonUi.normalText 16 "提案を作成中"
            ]
        ]


loadingView : Data.IdeaId -> Ui.Panel Message
loadingView (Data.IdeaId ideaIdAsString) =
    CommonUi.normalText 16 ("ideaId = " ++ ideaIdAsString ++ " を読込中")


notFoundView : Data.IdeaId -> Ui.Panel Message
notFoundView (Data.IdeaId ideaIdAsString) =
    CommonUi.normalText 16 ("ideaId = " ++ ideaIdAsString ++ " が見つからなかった")


mainView : Message.SubModel -> LoadedModel -> Ui.Panel Message
mainView subModel loadedModel =
    let
        (Data.IdeaId ideaIdAsString) =
            loadedModel.id
    in
    Ui.column
        (Ui.stretchWithMaxSize 800)
        Ui.auto
        [ Ui.gap 16 ]
        ([ CommonUi.subText ideaIdAsString
         , CommonUi.normalText 24 loadedModel.snapshot.name
         , CommonUi.normalText 16
            (if loadedModel.fresh then
                "最新のデータです"

             else
                "最新のデータを取得中"
            )
         , CommonUi.table
            [ ( "いいだしっぺ", CommonUi.userView subModel loadedModel.snapshot.createUserId )
            , ( "作成日時", CommonUi.timeView subModel loadedModel.snapshot.createTime )
            , ( "更新日時", CommonUi.timeView subModel loadedModel.snapshot.updateTime )
            , ( "取得日時", CommonUi.timeView subModel loadedModel.snapshot.getTime )
            ]
         , Ui.column
            Ui.stretch
            Ui.auto
            [ Ui.gap 8 ]
            (List.map (itemView subModel) loadedModel.snapshot.itemList)
         ]
            ++ commentInputView subModel
        )


itemView : Message.SubModel -> Data.IdeaItem -> Ui.Panel Message
itemView subModel ideaItem =
    Ui.row
        Ui.stretch
        Ui.auto
        []
        [ CommonUi.miniUserView subModel ideaItem.createUserId
        , Ui.column
            Ui.stretch
            Ui.auto
            []
            [ itemBodyView subModel ideaItem.body
            , CommonUi.timeView subModel ideaItem.createTime
            ]
        ]


itemBodyView : Message.SubModel -> Data.ItemBody -> Ui.Panel Message
itemBodyView subModel itemBody =
    case itemBody of
        Data.ItemBodyComment string ->
            CommonUi.stretchText 24 string

        Data.ItemBodySuggestionCreate suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                Nothing
                (CommonUi.normalText 16 "提案を作成した")

        Data.ItemBodySuggestionToApprovalPending suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                Nothing
                (CommonUi.normalText 16 "提案を承認待ちにした")

        Data.ItemBodySuggestionCancelToApprovalPending suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                Nothing
                (CommonUi.normalText 16 "承認待ちをキャンセルした")

        Data.ItemBodySuggestionApprove suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                Nothing
                (CommonUi.normalText 16 "提案を承認した")

        Data.ItemBodySuggestionReject suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                Nothing
                (CommonUi.normalText 16 "提案を拒否した")

        Data.ItemBodySuggestionCancelRejection suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                Nothing
                (CommonUi.normalText 16 "提案の拒否をキャンセルした")


commentInputView : Message.SubModel -> List (Ui.Panel Message)
commentInputView subModel =
    case Message.getLogInState subModel of
        Data.LogInState.Ok _ ->
            [ Ui.column
                Ui.stretch
                Ui.auto
                []
                [ Ui.textInput
                    Ui.stretch
                    Ui.auto
                    []
                    (Ui.TextInputAttributes
                        { inputMessage = InputComment
                        , name = "comment"
                        , id = "comment"
                        , multiLine = True
                        , fontSize = 16
                        }
                    )
                , CommonUi.button
                    Comment
                    "コメントする"
                ]
            , CommonUi.button
                Suggestion
                "編集提案をする"
            ]

        _ ->
            []
