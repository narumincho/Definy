import * as firestoreType from "definy-firestore-type";
import { Elm } from "../main/source/Main.elm";
import * as firebase from "firebase/app";
import "firebase/firestore";
import * as typedFirestore from "typed-firestore";
import * as sub from "./sub";
import * as serviceWorkerPostData from "../serviceWorkerPostData";

const elmAppElement = document.createElement("div");

// bodyの子要素を削除
document.documentElement.replaceChild(
  document.body.cloneNode(false),
  document.body
);
document.body.appendChild(elmAppElement);

const checkWebGLSupport = (): boolean => {
  try {
    const context = document.createElement("canvas").getContext("webgl");
    return context !== null;
  } catch (e) {
    return false;
  }
};

requestAnimationFrame(() => {
  const serviceWorkerSupport = "serviceWorker" in navigator;
  const app = Elm.Main.init({
    flags: {
      url: location.href,
      windowSize: {
        width: innerWidth,
        height: innerHeight
      },
      language: navigator.languages[0],
      networkConnection: navigator.onLine,
      indexedDBSupport: "indexedDB" in window,
      webGLSupport: checkWebGLSupport(),
      serviceWorkerSupport: serviceWorkerSupport
    },
    node: elmAppElement
  });

  if (serviceWorkerSupport) {
    navigator.serviceWorker.addEventListener("message", e => {
      const data: serviceWorkerPostData.ServiceWorkerToClientMessage = e.data;
      switch (data) {
        case "offlineFileLoadError":
          app.ports.serviceWorkerActivatedWithOutOfflineFiles.send(null);
          return;
        case "offlineFileLoaded":
          app.ports.serviceWorkerActivatedWithOfflineFiles.send(null);
          return;
        case "startOfflineFileLoading":
          app.ports.serviceWorkerLoadingOfflineFiles.send(null);
          return;
      }
    });
    navigator.serviceWorker
      .register("../serviceworker.ts", { scope: "/" })
      .then(
        () => {
          console.log("serviceWorkerを登録した!");
        },
        () => {
          console.log("serviceWorkerの登録に失敗しました");
        }
      );
  }

  let prevKeyEvent: KeyboardEvent;
  /* キー入力 */
  window.addEventListener("keydown", e => {
    prevKeyEvent = e;
    app.ports.keyPressed.send(e);
  });
  /*
   * 直前のキー入力のデフォルト動作を取り消す
   * なぜかElmのコンパイルをデバッグモードでやるとキー動作を防げない
   */
  app.ports.preventDefaultBeforeKeyEvent.subscribe(_ => {
    console.log("直前のキー入力のデフォルト動作を取り消す", prevKeyEvent);
    if (prevKeyEvent.currentTarget === null) {
      console.log(
        "キーイベントの送信先オブジェクトがない!キー動作を無効化できないと思われる"
      );
    }
    prevKeyEvent.preventDefault();
    app.ports.keyPrevented.send(null);
  });
  /* ウィンドウサイズを変えたら */
  addEventListener("resize", (): void => {
    app.ports.windowResize.send({
      width: innerWidth,
      height: innerHeight
    });
  });

  app.ports.requestAccessTokenFromIndexedDB.subscribe(() => {
    const userDBRequest: IDBOpenDBRequest = indexedDB.open("user", 1);

    userDBRequest.onupgradeneeded = event => {
      console.log("ユーザーデータのDBが更新された");
      const target = event.target as IDBOpenDBRequest;
      const db = target.result;
      db.createObjectStore("accessToken", {});
    };

    userDBRequest.onsuccess = event => {
      console.log("ユーザーデータのDBに接続成功!");
      const target = event.target as IDBOpenDBRequest;
      const db = target.result;
      console.log("db in success", db);
      const transaction = db.transaction("accessToken", "readonly");
      transaction.oncomplete = event => {
        console.log("アクセストークン読み込みのトランザクションが成功した");
        db.close();
      };
      transaction.onerror = event => {
        console.log("アクセストークン読み込みのトランザクションが失敗した");
        db.close();
      };
      const getRequest = transaction
        .objectStore("accessToken")
        .get("lastLogInUser");
      getRequest.onsuccess = event => {
        console.log("読み込み完了!");
        const request = event.target as IDBRequest;
        if (request.result === undefined) {
          app.ports.portResponseAccessTokenFromIndexedDB.send("");
          return;
        }
        if (typeof request.result === "string") {
          app.ports.portResponseAccessTokenFromIndexedDB.send(request.result);
          return;
        }
        app.ports.portResponseAccessTokenFromIndexedDB.send("error");
      };
      getRequest.onerror = event => {
        console.log("読み込み失敗");
        app.ports.portResponseAccessTokenFromIndexedDB.send("error");
      };
    };

    userDBRequest.onerror = event => {
      console.log("ユーザーデータのDBに接続できなかった");
    };
  });

  app.ports.writeAccessTokenToIndexedDB.subscribe(accessToken => {
    const userDBRequest: IDBOpenDBRequest = indexedDB.open("user", 1);

    userDBRequest.onupgradeneeded = event => {
      console.log("ユーザーデータのDBが更新された");
      const target = event.target as IDBOpenDBRequest;
      const db = target.result;
      db.createObjectStore("accessToken", {});
    };

    userDBRequest.onsuccess = event => {
      console.log("ユーザーデータのDBに接続成功!");
      const target = event.target as IDBOpenDBRequest;
      const db = target.result;
      const transaction = db.transaction("accessToken", "readwrite");
      transaction.oncomplete = event => {
        console.log("アクセストークン保存のトランザクションが成功した");
        db.close();
      };
      transaction.onerror = event => {
        console.log("アクセストークン保存のトランザクションが失敗した");
        db.close();
      };
      const putRequest = transaction
        .objectStore("accessToken")
        .put(accessToken, "lastLogInUser");

      putRequest.onsuccess = event => {
        console.log("書き込み完了!");
      };
      putRequest.onerror = event => {
        console.log("読み込み失敗");
      };
    };

    userDBRequest.onerror = event => {
      console.log("ユーザーデータのDBに接続できなかった");
    };
  });

  app.ports.consoleLog.subscribe(text => {
    console.warn(text);
  });

  addEventListener("languagechange", () => {
    app.ports.changeLanguage.send(navigator.languages[0]);
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
});

(async () => {
  firebase.initializeApp({
    apiKey: "AIzaSyAy7vTr9xBSF0d9pEWufU6EJd0AcUnANZk",
    authDomain: "definy-lang.firebaseapp.com",
    projectId: "definy-lang",
    storageBucket: "definy-lang.appspot.com"
  });
  const database = (firebase.firestore() as unknown) as typedFirestore.Firestore<
    firestoreType.Firestore
  >;
  const collection = database.collection("user");
  collection.onSnapshot(snapShot => {
    for (const doc of snapShot.docs) {
      console.log(doc.data());
    }
  });
})();

sub.consoleSub();
