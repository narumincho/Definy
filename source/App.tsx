import * as React from "react";
import * as api from "./api";
import * as core from "definy-core";
import * as indexedDB from "./indexedDB";
import * as resourceAllProjectIdList from "./resource";
import {
  AccessToken,
  Language,
  Location,
  LogInState,
  Maybe,
  OpenIdConnectProvider,
  Resource,
  UrlData,
  User,
  UserId,
} from "definy-core/source/data";
import { About } from "./About";
import { CreateProject } from "./CreateProject";
import { Debug } from "./Debug";
import { Home } from "./Home";
import { LoadingBox } from "./ui";
import { Model } from "./model";
import { SidePanel } from "./SidePanel";
import styled from "styled-components";

export const App: React.FC<{
  accessToken: Maybe<AccessToken>;
  initUrlData: UrlData;
}> = (prop) => {
  const [urlData, onJump] = React.useState<UrlData>(prop.initUrlData);
  const [logInState, setLogInState] = React.useState<LogInState>(
    prop.accessToken._ === "Just"
      ? LogInState.WaitVerifyingAccessToken(prop.accessToken.value)
      : LogInState.WaitLoadingAccessTokenFromIndexedDB
  );
  const {
    allProjectIdListMaybe,
    projectMap,
    userMap,
    imageMap,
    requestAllProject,
    requestProject,
    requestUser,
    setUser,
    requestImage,
  } = resourceAllProjectIdList.useProjectAllIdList();

  // ルーティング
  React.useEffect(() => {
    window.history.pushState(
      undefined,
      "",
      core.urlDataAndAccessTokenToUrl(urlData, Maybe.Nothing()).toString()
    );
    window.addEventListener("popstate", () => {
      onJump(
        core.urlDataAndAccessTokenFromUrl(new URL(window.location.href)).urlData
      );
    });
  }, [urlData]);

  // ログイン
  React.useEffect(logInEffect(logInState, urlData, setLogInState, setUser), [
    logInState,
  ]);

  const model: Model = {
    clientMode: urlData.clientMode,
    language: urlData.language,
    logInState,
    projectMap,
    userMap,
    imageMap,
    onJump,
    allProjectIdListMaybe,
    requestAllProject,
    requestProject,
    requestUser,
    requestImage,
  };

  switch (logInState._) {
    case "WaitRequestingLogInUrl":
    case "RequestingLogInUrl":
      return (
        <RequestingLogInUrl
          message={logInMessage(
            logInState.openIdConnectProvider,
            urlData.language
          )}
        />
      );
    case "JumpingToLogInPage":
      return (
        <RequestingLogInUrl
          message={jumpMessage(new URL(logInState.string), urlData.language)}
        />
      );
  }
  return (
    <NormalStyledDiv>
      <SidePanel
        model={model}
        onRequestLogIn={(provider) => {
          setLogInState(LogInState.WaitRequestingLogInUrl(provider));
        }}
      />
      <MainPanel location={urlData.location} model={model} />
    </NormalStyledDiv>
  );
};

const NormalStyledDiv = styled.div({
  height: "100%",
  display: "grid",
  gridTemplateColumns: "auto 1fr",
});

const LogInViewStyledDiv = styled.div({
  height: "100%",
  display: "grid",
  alignItems: "center",
  justifyItems: "center",
});

const RequestingLogInUrl: React.FC<{
  message: string;
}> = (prop) => (
  <LogInViewStyledDiv>
    <LoadingBox>{prop.message}</LoadingBox>
  </LogInViewStyledDiv>
);

const logInMessage = (
  provider: OpenIdConnectProvider,
  language: Language
): string => {
  switch (language) {
    case "English":
      return `Preparing to log in to ${provider}`;
    case "Esperanto":
      return `Preparante ensaluti al Google${provider}`;
    case "Japanese":
      return `${provider}へのログインを準備中……`;
  }
};

const jumpMessage = (url: URL, language: Language): string => {
  switch (language) {
    case "English":
      return `Navigating to ${url}`;
    case "Esperanto":
      return `navigante al ${url}`;
    case "Japanese":
      return `${url}へ移動中……`;
  }
};

const MainPanel: React.FC<{
  model: Model;
  location: Location;
}> = (prop) => {
  switch (prop.location._) {
    case "Home":
      return <Home model={prop.model} />;
    case "CreateProject":
      return <CreateProject model={prop.model} />;
    case "About":
      return <About />;
    case "Debug":
      return <Debug />;
    default:
      return <div>他のページは準備中</div>;
  }
};

const logInEffect = (
  logInState: LogInState,
  urlData: UrlData,
  dispatchLogInState: React.Dispatch<React.SetStateAction<LogInState>>,
  setUser: (userId: UserId, userResource: Resource<User>) => void
): React.EffectCallback => () => {
  switch (logInState._) {
    case "WaitLoadingAccessTokenFromIndexedDB":
      dispatchLogInState(LogInState.LoadingAccessTokenFromIndexedDB);
      indexedDB.getAccessToken().then((accessToken) => {
        if (accessToken === undefined) {
          dispatchLogInState(LogInState.Guest);
        } else {
          dispatchLogInState(LogInState.WaitVerifyingAccessToken(accessToken));
        }
      });
      return;
    case "Guest":
      return;
    case "WaitRequestingLogInUrl":
      dispatchLogInState(
        LogInState.RequestingLogInUrl(logInState.openIdConnectProvider)
      );
      api
        .requestLogInUrl({
          openIdConnectProvider: logInState.openIdConnectProvider,
          urlData,
        })
        .then((logInUrl) => {
          dispatchLogInState(LogInState.JumpingToLogInPage(logInUrl));
        });
      return;
    case "JumpingToLogInPage":
      window.location.href = logInState.string;
      return;
    case "WaitVerifyingAccessToken":
      dispatchLogInState({
        _: "VerifyingAccessToken",
        accessToken: logInState.accessToken,
      });
      api
        .getUserByAccessToken(logInState.accessToken)
        .then((userResourceAndIdMaybe) => {
          switch (userResourceAndIdMaybe._) {
            case "Just":
              indexedDB.setAccessToken(logInState.accessToken);
              dispatchLogInState(
                LogInState.LoggedIn({
                  accessToken: logInState.accessToken,
                  userId: userResourceAndIdMaybe.value.id,
                })
              );
              setUser(
                userResourceAndIdMaybe.value.id,
                userResourceAndIdMaybe.value.data
              );
              return;
            case "Nothing":
              dispatchLogInState(LogInState.Guest);
          }
        });
  }
};
