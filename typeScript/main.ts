import { Elm } from "../elm/source/Main.elm";
import * as common from "definy-common";
import { data } from "definy-common";
import * as db from "./indexedDB";

const elmAppElement = document.createElement("div");

// bodyの子要素を削除
document.documentElement.replaceChild(
  document.body.cloneNode(false),
  document.body
);
document.body.appendChild(elmAppElement);

const getAccessToken = async (
  database: IDBDatabase | null,
  accessTokenFromUrl: data.Maybe<data.AccessToken>
): Promise<data.Maybe<data.AccessToken>> => {
  switch (accessTokenFromUrl._) {
    case "Just":
      return data.maybeJust(accessTokenFromUrl.value);
    case "Nothing": {
      const accessToken = await db.getAccessToken(database);
      if (accessToken === undefined) {
        return data.maybeNothing();
      }
      return data.maybeJust(accessToken);
    }
  }
};

const callApi = <responseType>(
  apiName: string,
  binary: ReadonlyArray<number>,
  decodeFunction: (
    index: number,
    binary: Uint8Array
  ) => { result: responseType; nextIndex: number }
): Promise<responseType> =>
  fetch("https://us-central1-definy-lang.cloudfunctions.net/api/" + apiName, {
    method: "POST",
    body: new Uint8Array(binary),
    headers: [["content-type", "application/octet-stream"]],
  })
    .then((response) => response.arrayBuffer())
    .then((response) => decodeFunction(0, new Uint8Array(response)).result);

const init = async (): Promise<void> => {
  if ("serviceWorker" in navigator) {
    navigator.serviceWorker.register("sw.ts", { scope: "/" }).then(
      () => {
        console.log("serviceWorkerを登録した!");
      },
      () => {
        console.log("serviceWorkerの登録に失敗しました");
      }
    );
  }
  const database = await db.accessDatabase();

  const accessToken = await getAccessToken(
    database,
    common.urlDataAndAccessTokenFromUrl(new URL(location.href)).accessToken
  );
  if (accessToken._ === "Just") {
    db.setAccessToken(database, accessToken.value);
  }
  const app = Elm.Main.init({
    flags: {
      windowSize: {
        width: innerWidth,
        height: innerHeight,
      },
      nowTime: common.util.timeFromDate(new Date()),
      accessTokenMaybe: accessToken._ === "Just" ? accessToken.value : null,
      networkConnection: navigator.onLine,
    },
    node: elmAppElement,
  });

  let prevKeyEvent: KeyboardEvent;
  /* キー入力 */
  window.addEventListener("keydown", (e) => {
    prevKeyEvent = e;
    app.ports.keyPressed.send(e);
  });
  /*
   * 直前のキー入力のデフォルト動作を取り消す
   * なぜかElmのコンパイルをデバッグモードでやるとキー動作を防げない
   */
  // app.ports.preventDefaultBeforeKeyEvent.subscribe(() => {
  //   console.log("直前のキー入力のデフォルト動作を取り消す", prevKeyEvent);
  //   if (prevKeyEvent.currentTarget === null) {
  //     console.log(
  //       "キーイベントの送信先オブジェクトがない!キー動作を無効化できないと思われる"
  //     );
  //   }
  //   prevKeyEvent.preventDefault();
  //   app.ports.keyPrevented.send(null);
  // });
  /* ウィンドウサイズを変えたら */
  addEventListener("resize", (): void => {
    app.ports.windowResize.send({
      width: innerWidth,
      height: innerHeight,
    });
  });

  app.ports.consoleLog.subscribe((text) => {
    console.warn(text);
  });

  addEventListener("pointerup", () => {
    app.ports.subPointerUp.send(null);
  });

  document.addEventListener("visibilitychange", () => {
    if (document.visibilityState === "hidden") {
      app.ports.subPointerUp.send(null);
    }
  });

  addEventListener("online", () => {
    app.ports.changeNetworkConnection.send(true);
  });

  addEventListener("offline", () => {
    app.ports.changeNetworkConnection.send(false);
  });

  app.ports.requestLogInUrl.subscribe((requestData) => {
    callApi(
      "requestLogInUrl",
      data.encodeRequestLogInUrlRequestData(requestData),
      data.decodeString
    ).then((url) => {
      location.href = url;
    });
  });

  app.ports.getUserByAccessToken.subscribe((accessToken) => {
    callApi(
      "getUserByAccessToken",
      data.encodeToken(accessToken),
      data.decodeMaybe(data.decodeUserSnapshotAndId)
    ).then((maybeUserPublicAndUserId) => {
      app.ports.responseUserByAccessToken.send(maybeUserPublicAndUserId);
      if (maybeUserPublicAndUserId._ === "Just") {
        db.setUser(
          database,
          maybeUserPublicAndUserId.value.id,
          maybeUserPublicAndUserId.value.snapshot
        );
      }
    });
  });

  app.ports.getImageBlobUrl.subscribe((imageToken) => {
    db.getImage(database, imageToken).then((binaryInIndexDB) => {
      if (binaryInIndexDB !== undefined) {
        const blob = new Blob([binaryInIndexDB], { type: "image/png" });
        const blobUrl = URL.createObjectURL(blob);
        app.ports.getImageBlobResponse.send({
          blobUrl: blobUrl,
          imageToken: imageToken,
        });
        return;
      }
      callApi(
        "getImageFile",
        data.encodeToken(imageToken),
        data.decodeBinary
      ).then((pngBinary) => {
        const blob = new Blob([pngBinary], { type: "image/png" });
        const blobUrl = URL.createObjectURL(blob);
        db.setImage(database, imageToken, pngBinary);
        app.ports.getImageBlobResponse.send({
          blobUrl: blobUrl,
          imageToken: imageToken,
        });
      });
    });
  });
  app.ports.createProject.subscribe((parameter) => {
    callApi(
      "createProject",
      data.encodeCreateProjectParameter(parameter),
      data.decodeMaybe(common.data.decodeProjectSnapshotAndId)
    ).then((response) => {
      if (response._ === "Just") {
        db.setProject(database, response.value.id, response.value.snapshot);
      }
      app.ports.createProjectResponse.send(response);
      console.log("プロジェクト作成しました!", response);
    });
  });
  app.ports.createIdea.subscribe((parameter) => {
    callApi(
      "createIdea",
      data.encodeCreateIdeaParameter(parameter),
      data.decodeMaybe(data.decodeIdeaSnapshotAndId)
    ).then((response) => {
      if (response._ === "Just") {
        db.setIdea(database, response.value.id, response.value.snapshot);
      }
      app.ports.responseCreateIdea.send(response);
    });
  });
  app.ports.addComment.subscribe((parameter) => {
    callApi(
      "addComment",
      data.encodeAddCommentParameter(parameter),
      data.decodeMaybe(data.decodeIdeaSnapshot)
    ).then((response) => {
      if (response._ === "Just") {
        db.setIdea(database, parameter.ideaId, response.value);
      }
      app.ports.responseAddComment.send({
        id: parameter.ideaId,
        snapshotMaybe: response,
      });
    });
  });
  app.ports.addSuggestion.subscribe((parameter) => {
    callApi(
      "addSuggestion",
      data.encodeAddSuggestionParameter(parameter),
      data.decodeMaybe(data.decodeSuggestionSnapshotAndId)
    ).then((response) => {
      if (response._ === "Just") {
        db.setSuggestion(database, response.value.id, response.value.snapshot);
      }
      app.ports.responseAddSuggestion.send(response);
    });
  });

  app.ports.toValidProjectName.subscribe((projectName) => {
    app.ports.toValidProjectNameResponse.send({
      input: projectName,
      result: common.stringToValidProjectName(projectName),
    });
  });
  app.ports.toValidIdeaName.subscribe((ideaName) => {
    app.ports.toValidIdeaNameResponse.send({
      input: ideaName,
      result: common.stringToValidIdeaName(ideaName),
    });
  });
  app.ports.toValidTypePartName.subscribe((name) => {
    const result = common.stringToTypePartName(name);
    app.ports.toValidTypePartNameResponse.send({
      input: name,
      result: result === undefined ? null : result,
    });
  });

  app.ports.getAllProjectIdList.subscribe(() => {
    callApi("getAllProjectId", [], data.decodeList(data.decodeId)).then(
      (idList) => {
        const projectIdList = idList as ReadonlyArray<common.data.ProjectId>;
        app.ports.responseAllProjectId.send(projectIdList);
      }
    );
  });

  /**
   * Userのデータをサーバーに問い合わせて取得して,その結果をindexedDBに保存して返す
   */
  const getUserFromServerSetInIndexedDB = (
    userId: data.UserId
  ): Promise<data.UserResponse> =>
    callApi(
      "getUser",
      data.encodeId(userId),
      data.decodeMaybe(data.decodeUserSnapshot)
    ).then((userSnapshotFromServer) => {
      if (userSnapshotFromServer._ === "Just") {
        db.setUser(database, userId, userSnapshotFromServer.value);
      }
      return {
        id: userId,
        snapshotMaybe: userSnapshotFromServer,
      };
    });

  app.ports.getUser.subscribe((userId) => {
    db.getUser(database, userId).then((userSnapshotInIndexedDB) => {
      if (userSnapshotInIndexedDB !== undefined) {
        app.ports.responseUser.send({
          id: userId,
          snapshotMaybe: data.maybeJust(userSnapshotInIndexedDB),
        });
        return;
      }
      getUserFromServerSetInIndexedDB(userId).then(app.ports.responseUser.send);
    });
  });
  app.ports.getUserNoCache.subscribe((userId) => {
    getUserFromServerSetInIndexedDB(userId).then(app.ports.responseUser.send);
  });

  const getProjectFromServerSetInIndexedDB = (
    projectId: data.ProjectId
  ): Promise<data.ProjectResponse> =>
    callApi(
      "getProject",
      data.encodeId(projectId),
      data.decodeMaybe(data.decodeProjectSnapshot)
    ).then((projectMaybe) => {
      if (projectMaybe._ === "Just") {
        db.setProject(database, projectId, projectMaybe.value);
      }
      return {
        id: projectId,
        snapshotMaybe: projectMaybe,
      };
    });

  app.ports.getProject.subscribe((projectId) => {
    db.getProject(database, projectId).then((projectDataInIndexedDB) => {
      if (projectDataInIndexedDB !== undefined) {
        app.ports.responseProject.send({
          id: projectId,
          snapshotMaybe: data.maybeJust(projectDataInIndexedDB),
        });
        return;
      }
      getProjectFromServerSetInIndexedDB(projectId).then(
        app.ports.responseProject.send
      );
    });
  });
  app.ports.getProjectNoCache.subscribe((projectId) => {
    getProjectFromServerSetInIndexedDB(projectId).then(
      app.ports.responseProject.send
    );
  });

  app.ports.getIdeaAndIdListByProjectId.subscribe((projectId) => {
    callApi(
      "getIdeaAndIdListByProjectId",
      data.encodeId(projectId),
      data.decodeList(data.decodeIdeaSnapshotAndId)
    ).then((ideaSnapshotAndIdList) => {
      for (const ideaSnapshotAndId of ideaSnapshotAndIdList) {
        db.setIdea(database, ideaSnapshotAndId.id, ideaSnapshotAndId.snapshot);
      }
      console.log(ideaSnapshotAndIdList);
      app.ports.responseIdeaSnapshotAndIdListByProjectId.send({
        projectId: projectId,
        ideaSnapshotAndIdList: ideaSnapshotAndIdList,
      });
    });
  });

  const getIdeaFromServerSetInIndexedDB = (
    ideaId: data.IdeaId
  ): Promise<data.IdeaResponse> =>
    callApi(
      "getIdea",
      data.encodeId(ideaId),
      data.decodeMaybe(data.decodeIdeaSnapshot)
    ).then((ideaSnapshotMaybe) => {
      if (ideaSnapshotMaybe._ === "Just") {
        db.setIdea(database, ideaId, ideaSnapshotMaybe.value);
      }
      return {
        id: ideaId,
        snapshotMaybe: ideaSnapshotMaybe,
      };
    });
  app.ports.getIdea.subscribe((ideaId) => {
    db.getIdea(database, ideaId).then((ideaSnapshotInIndexedDB) => {
      if (ideaSnapshotInIndexedDB !== undefined) {
        app.ports.responseIdea.send({
          id: ideaId,
          snapshotMaybe: data.maybeJust(ideaSnapshotInIndexedDB),
        });
        return;
      }
      getIdeaFromServerSetInIndexedDB(ideaId).then(app.ports.responseIdea.send);
    });
  });
  app.ports.getIdeaNoCache.subscribe((ideaId) => {
    getIdeaFromServerSetInIndexedDB(ideaId).then(app.ports.responseIdea.send);
  });

  const getSuggestionFromServerSetInIndexedDB = (
    suggestionId: data.SuggestionId
  ): Promise<data.SuggestionResponse> =>
    callApi(
      "getSuggestion",
      data.encodeId(suggestionId),
      data.decodeMaybe(data.decodeSuggestionSnapshot)
    ).then((suggestionSnapshotMaybe) => {
      if (suggestionSnapshotMaybe._ === "Just") {
        db.setSuggestion(database, suggestionId, suggestionSnapshotMaybe.value);
      }

      return {
        id: suggestionId,
        snapshotMaybe: suggestionSnapshotMaybe,
      };
    });
  app.ports.getSuggestion.subscribe((suggestionId) => {
    db.getSuggestion(database, suggestionId).then((suggestionInIndexedDB) => {
      if (suggestionInIndexedDB !== undefined) {
        app.ports.responseSuggestion.send({
          id: suggestionId,
          snapshotMaybe: data.maybeJust(suggestionInIndexedDB),
        });
      }
      getSuggestionFromServerSetInIndexedDB(suggestionId).then(
        app.ports.responseSuggestion.send
      );
    });
  });
  app.ports.getSuggestionNoCache.subscribe((suggestionId) => {
    getSuggestionFromServerSetInIndexedDB(suggestionId).then(
      app.ports.responseSuggestion.send
    );
  });
};

requestAnimationFrame(() => {
  init();
});
