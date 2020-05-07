module Data.Resource exposing (User(..), updateByUserResponse)

import Data
import Data.TimeZoneAndName
import Message


{-| ユーザーの読み込んだデータと読み込んだ段階
-}
type User
    = Loading Data.UserId
    | Loaded Data.UserId Data.UserSnapshot
    | NotFound Data.UserId


userGetUserId : User -> Data.UserId
userGetUserId user =
    case user of
        Loading userId ->
            userId

        Loaded userId _ ->
            userId

        NotFound userId ->
            userId


{-| 取得したユーザーが必要でなければ無視をして, ユーザーの情報が新しければ, ユーザーの画像をリクエストをして, 古かったらキャッシュを使用しないでユーザーの情報をリクエストする
-}
updateByUserResponse : Message.SubModel -> User -> Data.UserResponse -> ( User, Message.Command )
updateByUserResponse subModel user userResponse =
    if userGetUserId user == userResponse.id then
        case userResponse.snapshotMaybe of
            Just snapshot ->
                ( Loaded userResponse.id snapshot
                , Message.GetBlobUrl snapshot.imageHash
                )

            Nothing ->
                ( NotFound userResponse.id
                , Message.None
                )

    else
        ( user
        , Message.None
        )


userIsFresh : Message.SubModel -> Data.UserSnapshot -> Bool
userIsFresh subModel userSnapshot =
    Data.TimeZoneAndName.isFresh 5000 (Message.getNowTime subModel) userSnapshot.getTime
