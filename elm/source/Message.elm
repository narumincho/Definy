module Message exposing (Command(..), CommonCommand(..), CommonMessage(..), SubModel, WindowSize, addImageBlobUrl, from, getClientMode, getImageBlobUrl, getLanguage, getLogInState, getNowTime, getTimeZoneAndNameMaybe, getWindowSize, setClientMode, setLanguageAndClientMode, setLogInState, setNowTime, setTimeZoneAndName, setWindowSize, urlDataSameLanguageClientMode)

import Data
import Data.LogInState
import Data.TimeZoneAndName
import Dict
import Time


{-| 各ページの共通のレスポンス Message
-}
type CommonMessage
    = ResponseUser Data.UserResponse
    | ResponseProject Data.ProjectResponse
    | ResponseIdea Data.IdeaResponse
    | ResponseSuggestion Data.SuggestionResponse
    | ResponseAddSuggestion (Maybe Data.SuggestionSnapshotAndId)
    | ResponseAllProjectIdList (List Data.ProjectId)
    | ResponseIdeaListByProjectId Data.IdeaListByProjectIdResponse
    | UpdateTime
    | CommonCommand CommonCommand


{-| 各ページに送る共通の動作
-}
type CommonCommand
    = SelectUp
    | SelectDown
    | SelectLeft
    | SelectRight
    | SelectFirstChild
    | SelectLastChild
    | SelectParent
    | SelectRoot


{-| 各ページに渡すべきModel
-}
type SubModel
    = SubModel
        { logInState : Data.LogInState.LogInState
        , language : Data.Language
        , clientMode : Data.ClientMode
        , imageFileBlobDict : Dict.Dict String String
        , timeZoneAndNameMaybe : Maybe Data.TimeZoneAndName.TimeZoneAndName
        , nowTime : Time.Posix
        , windowSize : WindowSize
        }


type alias WindowSize =
    { width : Int
    , height : Int
    }


from :
    { logInState : Data.LogInState.LogInState
    , language : Data.Language
    , clientMode : Data.ClientMode
    , timeZoneAndNameMaybe : Maybe Data.TimeZoneAndName.TimeZoneAndName
    , nowTime : Time.Posix
    , windowSize : WindowSize
    }
    -> SubModel
from record =
    SubModel
        { logInState = record.logInState
        , language = record.language
        , clientMode = record.clientMode
        , imageFileBlobDict = Dict.empty
        , timeZoneAndNameMaybe = record.timeZoneAndNameMaybe
        , nowTime = record.nowTime
        , windowSize = record.windowSize
        }


getLogInState : SubModel -> Data.LogInState.LogInState
getLogInState (SubModel record) =
    record.logInState


setLogInState : Data.LogInState.LogInState -> SubModel -> SubModel
setLogInState logInState (SubModel record) =
    SubModel { record | logInState = logInState }


getLanguage : SubModel -> Data.Language
getLanguage (SubModel record) =
    record.language


getClientMode : SubModel -> Data.ClientMode
getClientMode (SubModel record) =
    record.clientMode


setClientMode : Data.ClientMode -> SubModel -> SubModel
setClientMode clientMode (SubModel record) =
    SubModel
        { record | clientMode = clientMode }


setLanguageAndClientMode : Data.Language -> Data.ClientMode -> SubModel -> SubModel
setLanguageAndClientMode language clientMode (SubModel record) =
    SubModel
        { record | language = language, clientMode = clientMode }


addImageBlobUrl : Data.ImageToken -> String -> SubModel -> SubModel
addImageBlobUrl (Data.ImageToken hash) blobUrl (SubModel record) =
    SubModel
        { record | imageFileBlobDict = Dict.insert hash blobUrl record.imageFileBlobDict }


getTimeZoneAndNameMaybe : SubModel -> Maybe Data.TimeZoneAndName.TimeZoneAndName
getTimeZoneAndNameMaybe (SubModel record) =
    record.timeZoneAndNameMaybe


setTimeZoneAndName : Data.TimeZoneAndName.TimeZoneAndName -> SubModel -> SubModel
setTimeZoneAndName timeZoneAndName (SubModel record) =
    SubModel { record | timeZoneAndNameMaybe = Just timeZoneAndName }


getNowTime : SubModel -> Time.Posix
getNowTime (SubModel record) =
    record.nowTime


setNowTime : Time.Posix -> SubModel -> SubModel
setNowTime timePosix (SubModel record) =
    SubModel { record | nowTime = timePosix }


getWindowSize : SubModel -> WindowSize
getWindowSize (SubModel record) =
    record.windowSize


setWindowSize : WindowSize -> SubModel -> SubModel
setWindowSize windowSize (SubModel record) =
    SubModel { record | windowSize = windowSize }


getImageBlobUrl : Data.ImageToken -> SubModel -> Maybe String
getImageBlobUrl (Data.ImageToken hash) (SubModel record) =
    Dict.get hash record.imageFileBlobDict


{-| SubModelからClientModeとLanguageを読んで場所を加えたURL Dataを作る
-}
urlDataSameLanguageClientMode : Data.Location -> SubModel -> Data.UrlData
urlDataSameLanguageClientMode location subModel =
    { clientMode = getClientMode subModel
    , language = getLanguage subModel
    , location = location
    }


{-| 各ページの共通のCmd
-}
type Command
    = None
    | GetBlobUrl Data.ImageToken
    | CreateProject String
    | CreateIdea { projectId : Data.ProjectId, ideaName : String }
    | AddComment { ideaId : Data.IdeaId, comment : String }
    | AddSuggestion Data.IdeaId
    | ConsoleLog String
    | PushUrl Data.UrlData
    | ToValidProjectName String
    | ToValidIdeaName String
    | GetAllProjectId
    | GetProject Data.ProjectId
    | GetProjectNoCache Data.ProjectId
    | GetUser Data.UserId
    | GetUserNoCache Data.UserId
    | GetIdea Data.IdeaId
    | GetIdeaNoCache Data.IdeaId
    | GetSuggestion Data.SuggestionId
    | GetSuggestionNoCache Data.SuggestionId
    | GetIdeaListByProjectId Data.ProjectId
    | FocusElement String
    | Batch (List Command)
