module Page.Project exposing (Message, Model, getProjectId, init, update, updateByCommonMessage, view)

import CommonUi
import Css
import Data
import Message
import Ui


type Model
    = Loading Data.ProjectId
    | NotFound Data.ProjectId
    | Loaded LoadedModel


type alias LoadedModel =
    { id : Data.ProjectId
    , snapshot : Data.ProjectSnapshot
    , ideaList : Maybe (List Data.IdeaSnapshotAndId)
    }


type Message
    = RequestLogInUrl Data.OpenIdConnectProvider


init : Data.ProjectId -> ( Model, Message.Command )
init projectId =
    ( Loading projectId
    , Message.GetProject projectId
    )


getProjectId : Model -> Data.ProjectId
getProjectId model =
    case model of
        Loading projectId ->
            projectId

        NotFound projectId ->
            projectId

        Loaded { id } ->
            id


updateByCommonMessage : Message.CommonMessage -> Model -> ( Model, Message.Command )
updateByCommonMessage message model =
    case ( message, model ) of
        ( Message.ResponseProject response, _ ) ->
            if response.id == getProjectId model then
                case response.snapshotMaybe of
                    Just projectSnapshot ->
                        ( Loaded
                            { id = response.id
                            , snapshot = projectSnapshot
                            , ideaList = Nothing
                            }
                        , Message.Batch
                            [ Message.GetBlobUrl projectSnapshot.imageHash
                            , Message.GetBlobUrl projectSnapshot.iconHash
                            , Message.GetUser projectSnapshot.createUserId
                            , Message.GetIdeaListByProjectId (getProjectId model)
                            ]
                        )

                    Nothing ->
                        ( NotFound response.id
                        , Message.None
                        )

            else
                ( model, Message.None )

        ( Message.ResponseIdeaListByProjectId response, Loaded snapshotAndId ) ->
            let
                _ =
                    Debug.log "response" response
            in
            if getProjectId model == response.projectId then
                ( Loaded
                    { snapshotAndId | ideaList = Just response.ideaSnapshotAndIdList }
                , Message.ConsoleLog "OK"
                )

            else
                ( model
                , Message.None
                )

        ( _, _ ) ->
            ( model
            , Message.None
            )


update : Message -> Model -> ( Model, Message.Command )
update message model =
    case message of
        RequestLogInUrl provider ->
            ( model
            , Message.RequestLogInUrl provider
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
                    CommonUi.Project
                        { id = loadedModel.id
                        , snapshot = loadedModel.snapshot
                        }

                _ ->
                    CommonUi.None
            )
            |> Ui.map RequestLogInUrl
        , Ui.scroll
            Ui.stretch
            Ui.stretch
            []
            (Ui.column
                Ui.stretch
                Ui.auto
                []
                [ case model of
                    Loading projectId ->
                        loadingView projectId

                    NotFound projectId ->
                        notFoundView projectId

                    Loaded loadedModel ->
                        normalView subModel loadedModel
                ]
            )
        ]


loadingView : Data.ProjectId -> Ui.Panel message
loadingView (Data.ProjectId projectIdAsString) =
    CommonUi.normalText 24
        ("projectId = " ++ projectIdAsString ++ "のプロジェクトを読込中")


normalView : Message.SubModel -> LoadedModel -> Ui.Panel Message
normalView subModel loadedModel =
    let
        (Data.ProjectId projectIdAsString) =
            loadedModel.id
    in
    Ui.column
        (Ui.stretchWithMaxSize 800)
        Ui.auto
        [ Ui.gap 8, Ui.padding 8 ]
        [ CommonUi.subText projectIdAsString
        , projectNameView subModel loadedModel.snapshot.iconHash loadedModel.snapshot.name
        , imageView subModel loadedModel.snapshot.imageHash loadedModel.snapshot.name
        , createUserView subModel loadedModel.snapshot.createUserId
        , createTimeView subModel loadedModel.snapshot.createTime
        , updateTimeView subModel loadedModel.snapshot.updateTime
        , getTimeView subModel loadedModel.snapshot.getTime
        , ideaListView subModel loadedModel.id loadedModel.ideaList
        ]


projectNameView : Message.SubModel -> Data.ImageToken -> String -> Ui.Panel message
projectNameView sugModel iconFileHash name =
    Ui.row
        Ui.stretch
        Ui.auto
        []
        [ case Message.getImageBlobUrl iconFileHash sugModel of
            Just blobUrl ->
                Ui.bitmapImage
                    (Ui.fix 32)
                    (Ui.fix 32)
                    []
                    (Ui.BitmapImageAttributes
                        { blobUrl = blobUrl
                        , fitStyle = Ui.Contain
                        , alternativeText = name ++ "のアイコン"
                        , rendering = Ui.ImageRenderingAuto
                        }
                    )

            Nothing ->
                Ui.empty
                    (Ui.fix 32)
                    (Ui.fix 32)
                    []
        , CommonUi.normalText 16 name
        ]


imageView : Message.SubModel -> Data.ImageToken -> String -> Ui.Panel message
imageView subModel imageToken projectName =
    case Message.getImageBlobUrl imageToken subModel of
        Just blobUrl ->
            Ui.bitmapImage
                (Ui.stretchWithMaxSize 640)
                Ui.auto
                []
                (Ui.BitmapImageAttributes
                    { blobUrl = blobUrl
                    , fitStyle = Ui.Contain
                    , alternativeText = projectName ++ "のアイコン"
                    , rendering = Ui.ImageRenderingAuto
                    }
                )

        Nothing ->
            Ui.depth
                (Ui.stretchWithMaxSize 640)
                Ui.auto
                []
                [ ( ( Ui.Center, Ui.Center )
                  , CommonUi.normalText 16 (projectName ++ "の画像を読込中")
                  )
                ]


ideaListView : Message.SubModel -> Data.ProjectId -> Maybe (List Data.IdeaSnapshotAndId) -> Ui.Panel Message
ideaListView subModel projectId ideaSnapshotAndIdListMaybe =
    Ui.column
        Ui.stretch
        Ui.auto
        [ Ui.gap 16 ]
        ([ CommonUi.stretchText 24 "アイデア"
         , CommonUi.sameLanguageLink
            Ui.stretch
            Ui.auto
            [ Ui.padding 8
            , Ui.backgroundColor (Css.rgb 20 20 20)
            ]
            subModel
            (Data.LocationCreateIdea projectId)
            Nothing
            (CommonUi.normalText 16 "アイデアを作成する")
         ]
            ++ (case ideaSnapshotAndIdListMaybe of
                    Just ideaSnapshotAndIdList ->
                        List.map
                            (ideaItemView subModel)
                            ideaSnapshotAndIdList

                    Nothing ->
                        [ CommonUi.normalText 16 "アイデア一覧を読込中……" ]
               )
        )


ideaItemView : Message.SubModel -> Data.IdeaSnapshotAndId -> Ui.Panel message
ideaItemView subModel ideaSnapshotAndId =
    CommonUi.sameLanguageLink
        Ui.stretch
        Ui.auto
        []
        subModel
        (Data.LocationIdea ideaSnapshotAndId.id)
        Nothing
        (Ui.column
            Ui.stretch
            Ui.auto
            []
            [ CommonUi.stretchText 24 ideaSnapshotAndId.snapshot.name
            , Ui.row
                Ui.stretch
                Ui.auto
                []
                [ CommonUi.normalText 16 "更新日時:"
                , CommonUi.timeView subModel ideaSnapshotAndId.snapshot.updateTime
                ]
            ]
        )


notFoundView : Data.ProjectId -> Ui.Panel Message
notFoundView (Data.ProjectId projectIdAsString) =
    CommonUi.normalText 24
        ("projectId = " ++ projectIdAsString ++ "のプロジェクトを見つからなかった")


createUserView : Message.SubModel -> Data.UserId -> Ui.Panel Message
createUserView subModel userId =
    Ui.row
        Ui.stretch
        Ui.auto
        [ Ui.gap 8 ]
        [ CommonUi.normalText 16 "作成者:"
        , CommonUi.userView subModel userId
        ]


createTimeView : Message.SubModel -> Data.Time -> Ui.Panel Message
createTimeView subModel time =
    Ui.row
        Ui.stretch
        Ui.auto
        [ Ui.gap 8 ]
        [ CommonUi.normalText 16 "作成日時:"
        , CommonUi.timeView subModel time
        ]


updateTimeView : Message.SubModel -> Data.Time -> Ui.Panel Message
updateTimeView subModel time =
    Ui.row
        Ui.stretch
        Ui.auto
        [ Ui.gap 8 ]
        [ CommonUi.normalText 16 "更新日時:"
        , CommonUi.timeView subModel time
        ]


getTimeView : Message.SubModel -> Data.Time -> Ui.Panel Message
getTimeView subModel time =
    Ui.row
        Ui.stretch
        Ui.auto
        [ Ui.gap 8 ]
        [ CommonUi.normalText 16 "取得日時:"
        , CommonUi.timeView subModel time
        ]
