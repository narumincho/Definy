module Page.Idea exposing (Message(..), Model, getIdeaId, init, update, updateByCommonMessage, view)

import CommonUi
import Data
import Data.LogInState
import Data.Resource
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
    , snapshot : Idea
    , comment : Comment
    , fresh : Bool
    }


type Idea
    = Idea
        { name : String
        , createUser : Data.Resource.User
        , createTime : Data.Time
        , projectId : Data.ProjectId
        , itemList : List IdeaItem
        , updateTime : Data.Time
        , getTime : Data.Time
        }


type IdeaItem
    = IdeaItem
        { createUserId : Data.Resource.User
        , createTime : Data.Time
        , body : Data.ItemBody
        }


type Comment
    = Inputting String
    | Sending


type Message
    = InputComment String
    | Comment
    | Suggestion


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
        Message.ResponseUser user ->
            32

        Message.ResponseIdea idea ->
            if idea.id == getIdeaId model then
                case idea.snapshotMaybe of
                    Just snapshot ->
                        let
                            ( ideaSnapshot, command, fresh ) =
                                ideaSnapshotToIdea subModel idea.id snapshot
                        in
                        ( Loaded
                            { id = idea.id
                            , snapshot = ideaSnapshot
                            , comment = Inputting ""
                            , fresh = fresh
                            }
                        , command
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


ideaSnapshotToIdea : Message.SubModel -> Data.IdeaId -> Data.IdeaSnapshot -> ( Idea, Message.Command, Bool )
ideaSnapshotToIdea subModel ideaId ideaSnapshot =
    let
        idea : Idea
        idea =
            Idea
                { name = ideaSnapshot.name
                , createUser = CommonUi.Loading ideaSnapshot.createUser
                , createTime = ideaSnapshot.createTime
                , projectId = ideaSnapshot.projectId
                , itemList =
                    List.map
                        (\item ->
                            IdeaItem
                                { createUserId = CommonUi.Loading item.createUserId
                                , createTime = item.createTime
                                , body = item.body
                                }
                        )
                        ideaSnapshot.itemList
                , updateTime = ideaSnapshot.updateTime
                , getTime = ideaSnapshot.getTime
                }

        fresh : Bool
        fresh =
            isFresh subModel idea
    in
    ( idea
    , if fresh then
        Message.Batch
            (Message.GetUser ideaSnapshot.createUser
                :: List.map Message.GetUser
                    (List.map .createUserId ideaSnapshot.itemList)
            )

      else
        Message.GetIdeaNoCache ideaId
    , fresh
    )


isFresh : Message.SubModel -> Idea -> Bool
isFresh subModel (Idea { getTime }) =
    Data.TimeZoneAndName.isFresh 5000 (Message.getNowTime subModel) getTime


update : Message -> Model -> ( Model, Message.Command )
update message model =
    case ( message, model ) of
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
    Ui.column
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
            [ ( "いいだしっぺ", CommonUi.userView subModel loadedModel.snapshot.createUser )
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
                (CommonUi.normalText 16 "提案を作成した")

        Data.ItemBodySuggestionToApprovalPending suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                (CommonUi.normalText 16 "提案を承認待ちにした")

        Data.ItemBodySuggestionCancelToApprovalPending suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                (CommonUi.normalText 16 "承認待ちをキャンセルした")

        Data.ItemBodySuggestionApprove suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                (CommonUi.normalText 16 "提案を承認した")

        Data.ItemBodySuggestionReject suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
                (CommonUi.normalText 16 "提案を拒否した")

        Data.ItemBodySuggestionCancelRejection suggestionId ->
            CommonUi.sameLanguageLink
                Ui.stretch
                Ui.auto
                []
                subModel
                (Data.LocationSuggestion suggestionId)
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
