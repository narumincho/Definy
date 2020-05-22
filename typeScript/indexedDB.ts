import { data, util } from "definy-common";

const accessTokenObjectStoreName = "accessToken";
const accessTokenKeyName = "lastLogInUser";
const userObjectStoreName = "user";
const projectObjectStoreName = "project";
const fileObjectStoreName = "file";
const ideaObjectStoreName = "idea";
const suggestionObjectStoreName = "suggestion";
const partObjectStoreName = "part";
const typePartObjectStoreName = "typePart";

/**
 * ブラウザでindexDBがサポートされているかどうか調べる
 */
const checkIndexDBSupport = (): boolean => "indexedDB" in window;

/**
 * データベースにアクセスする.
 * ブラウザがindexedDBがサポートされていない場合,nullが返る
 * Databaseにアクセスできなかったとき,rejectされる
 */
export const accessDatabase = (): Promise<IDBDatabase | null> =>
  new Promise<IDBDatabase | null>((resolve, reject) => {
    if (!checkIndexDBSupport()) {
      resolve(null);
    }
    const dbRequest: IDBOpenDBRequest = indexedDB.open("main", 1);

    dbRequest.onupgradeneeded = (): void => {
      console.log("Databaseのversionが上がった");
      const db = dbRequest.result;
      db.createObjectStore(accessTokenObjectStoreName, {});
      db.createObjectStore(userObjectStoreName, {});
      db.createObjectStore(projectObjectStoreName, {});
      db.createObjectStore(fileObjectStoreName, {});
      db.createObjectStore(ideaObjectStoreName, {});
      db.createObjectStore(suggestionObjectStoreName, {});
      db.createObjectStore(partObjectStoreName, {});
      db.createObjectStore(typePartObjectStoreName, {});
    };

    dbRequest.onsuccess = (): void => {
      resolve(dbRequest.result);
    };

    dbRequest.onerror = (): void => {
      console.log("Databaseに接続できなかった");
      reject();
    };
  });
/**
 * indexDBからアクセストークンを取得する
 */
export const getAccessToken = (
  database: IDBDatabase | null
): Promise<undefined | data.AccessToken> =>
  get<typeof accessTokenKeyName, data.AccessToken>(
    database,
    accessTokenObjectStoreName,
    accessTokenKeyName
  );

/**
 * indexDBにアクセストークンを書き込む
 * @param database nullだった場合サポートされていないとみなされ常に何も取得できない
 * @param accessToken アクセストークン
 */
export const setAccessToken = (
  database: IDBDatabase | null,
  accessToken: data.AccessToken
): Promise<void> =>
  setLow<typeof accessTokenKeyName, data.AccessToken>(
    database,
    accessTokenObjectStoreName,
    accessTokenKeyName,
    accessToken
  );

/**
 * ユーザーのデータをindexedDBから読む
 */
export const getUser = (
  database: IDBDatabase | null,
  userId: data.UserId
): Promise<undefined | data.UserSnapshot> =>
  get<data.UserId, data.UserSnapshot>(database, userObjectStoreName, userId);

/**
 * 指定したユーザーIDのスナップショットがなかった場合, 指定したユーザースナップショットをindexedDBに書く
 * 前にあったユーザースナップショットのgetTimeより新しかった場合, 指定したユーザースナップショットをindexedDBに書く
 * そうでなければ何もしない
 */
export const setUser = (
  database: IDBDatabase | null,
  userId: data.UserId,
  userSnapshot: data.UserSnapshot
): Promise<void> =>
  set<data.UserId, data.UserSnapshot>(
    database,
    userObjectStoreName,
    userId,
    userSnapshot
  );

/**
 * プロジェクトのデータをindexedDBから読む
 */
export const getProject = (
  database: IDBDatabase | null,
  projectId: data.ProjectId
): Promise<undefined | data.ProjectSnapshot> =>
  get<data.ProjectId, data.ProjectSnapshot>(
    database,
    projectObjectStoreName,
    projectId
  );

/**
 * プロジェクトのスナップショットをindexedDBに書く
 *
 * 指定したプロジェクトIDのプロジェクトスナップショットがなかった場合, 指定したプロジェクトスナップショットをindexedDBに書く
 * 前にあったプロジェクトスナップショットのgetTimeより新しかった場合, 指定したプロジェクトスナップショットをindexedDBに書く
 * そうでなければ何もしない
 */
export const setProject = (
  database: IDBDatabase | null,
  projectId: data.ProjectId,
  projectSnapshot: data.ProjectSnapshot
): Promise<void> =>
  set<data.ProjectId, data.ProjectSnapshot>(
    database,
    projectObjectStoreName,
    projectId,
    projectSnapshot
  );

/**
 * ファイルのバイナリを読み込む
 */
export const getImage = (
  database: IDBDatabase | null,
  imageToken: data.ImageToken
): Promise<undefined | Uint8Array> =>
  get<data.ImageToken, Uint8Array>(database, fileObjectStoreName, imageToken);

/**
 * ファイルのバイナリを書き込む
 */
export const setImage = (
  database: IDBDatabase | null,
  imageToken: data.ImageToken,
  image: Uint8Array
): Promise<void> =>
  setLow<data.ImageToken, Uint8Array>(
    database,
    fileObjectStoreName,
    imageToken,
    image
  );

export const getIdea = (
  database: IDBDatabase | null,
  ideaId: data.IdeaId
): Promise<undefined | data.IdeaSnapshot> =>
  get<data.IdeaId, data.IdeaSnapshot>(database, ideaObjectStoreName, ideaId);

/**
 * アイデアのスナップショットをindexedDBに書く
 *
 * 指定したアイデアIDのアイデアスナップショットがなかった場合, 指定したアイデアスナップショットをindexedDBに書く
 * 前にあったアイデアスナップショットのgetTimeより新しかった場合, 指定したアイデアスナップショットをindexedDBに書く
 * そうでなければ何もしない
 */
export const setIdea = (
  database: IDBDatabase | null,
  id: data.IdeaId,
  snapshot: data.IdeaSnapshot
): Promise<void> =>
  set<data.IdeaId, data.IdeaSnapshot>(
    database,
    ideaObjectStoreName,
    id,
    snapshot
  );

export const getSuggestion = (
  database: IDBDatabase | null,
  id: data.SuggestionId
): Promise<data.SuggestionSnapshot | undefined> =>
  get<data.SuggestionId, data.SuggestionSnapshot>(
    database,
    suggestionObjectStoreName,
    id
  );

export const setSuggestion = (
  database: IDBDatabase | null,
  id: data.SuggestionId,
  snapshot: data.SuggestionSnapshot
): Promise<void> =>
  set<data.SuggestionId, data.SuggestionSnapshot>(
    database,
    suggestionObjectStoreName,
    id,
    snapshot
  );

export const getPart = (
  database: IDBDatabase | null,
  id: data.PartId
): Promise<data.PartSnapshot | undefined> =>
  get<data.PartId, data.PartSnapshot>(database, partObjectStoreName, id);

export const setPart = (
  database: IDBDatabase | null,
  id: data.PartId,
  snapshot: data.PartSnapshot
): Promise<void> =>
  set<data.PartId, data.PartSnapshot>(
    database,
    partObjectStoreName,
    id,
    snapshot
  );

export const getTypePart = (
  database: IDBDatabase | null,
  id: data.TypePartId
): Promise<data.TypePartSnapshot | undefined> =>
  get<data.TypePartId, data.TypePartSnapshot>(
    database,
    typePartObjectStoreName,
    id
  );

export const setTypePart = (
  database: IDBDatabase | null,
  id: data.TypePartId,
  snapshot: data.TypePartSnapshot
): Promise<void> =>
  set<data.TypePartId, data.TypePartSnapshot>(
    database,
    typePartObjectStoreName,
    id,
    snapshot
  );

const get = <id extends string, data>(
  database: IDBDatabase | null,
  objectStoreName: string,
  id: id
): Promise<data | undefined> =>
  new Promise((resolve, reject) => {
    if (database === null) {
      resolve();
      return;
    }
    const transaction = database.transaction([objectStoreName], "readwrite");

    transaction.oncomplete = (): void => {
      resolve(getRequest.result);
    };

    transaction.onerror = (): void => {
      reject("read " + objectStoreName + " failed");
    };

    const getRequest: IDBRequest<data | undefined> = transaction
      .objectStore(objectStoreName)
      .get(id);
  });

/**
 * データをindexedDBに書く
 *
 * 指定したIDのデータがなかった場合, 指定したデータをindexedDBに書く
 * 指定したデータのgetTimeが 前にあったデータのgetTimeより新しかった場合, 指定したデータをindexedDBに書く
 * そうでなければ何もしない
 */
const set = <id extends string, data extends { getTime: data.Time }>(
  database: IDBDatabase | null,
  objectStoreName: string,
  id: id,
  data: data
): Promise<void> =>
  new Promise((resolve, reject) => {
    if (database === null) {
      resolve();
      return;
    }

    const transaction = database.transaction([objectStoreName], "readonly");

    transaction.oncomplete = (): void => {
      if (getRequest.result === undefined) {
        setLow(database, objectStoreName, id, data).then(resolve);
        return;
      }
      if (
        util.timeToDate(getRequest.result.getTime).getTime() <
        util.timeToDate(data.getTime).getTime()
      ) {
        setLow(database, objectStoreName, id, data).then(resolve);
        return;
      }
      resolve();
    };

    transaction.onerror = (): void => {
      reject("set before get getTime " + objectStoreName + " failed");
    };

    const getRequest: IDBRequest<undefined | data> = transaction
      .objectStore(objectStoreName)
      .get(id);
  });

const setLow = <id extends string, data>(
  database: IDBDatabase | null,
  objectStoreName: string,
  id: id,
  data: data
): Promise<void> =>
  new Promise((resolve, reject) => {
    if (database === null) {
      resolve();
      return;
    }

    const transaction = database.transaction([objectStoreName], "readwrite");

    transaction.oncomplete = (): void => {
      resolve();
    };

    transaction.onerror = (): void => {
      reject("write " + objectStoreName + " error: write transaction failed");
    };

    transaction.objectStore(objectStoreName).put(data, id);
  });
