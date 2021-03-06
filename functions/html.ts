import * as commonUrl from "../common/url";
import * as d from "../data";
import * as lib from "./lib";
import * as nHtml from "@narumincho/html";
import * as out from "../out";

/**
 * OGP の 情報が含まれている HTML を返す
 */
export const html = async (
  urlData: d.UrlData,
  normalizedUrl: URL
): Promise<{ view: nHtml.view.View<never>; isNotFound: boolean }> => {
  const coverImageUrlAndDescription = await getCoverImageUrlAndDescription(
    urlData.location,
    urlData.language
  );
  return {
    view: {
      appName: "Definy",
      pageName: "Definy",
      iconPath: "/icon",
      coverImageUrl: coverImageUrlAndDescription.imageUrl,
      description: coverImageUrlAndDescription.description,
      scriptPath: "/main.js",
      styleUrlList: [],
      twitterCard: "SummaryCard",
      language: urlData.language,
      manifestPath: ["manifest.json"],
      url: new URL(normalizedUrl.toString()),
      style: `/*
    Hack typeface https://github.com/source-foundry/Hack
    License: https://github.com/source-foundry/Hack/blob/master/LICENSE.md
*/

@font-face {
    font-family: "Hack";
    font-weight: 400;
    font-style: normal;
    src: url("/hack-regular-subset.woff2") format("woff2");
}

html {
    height: 100%;
}

body {
    height: 100%;
    margin: 0;
    background-color: black;
    display: grid;
}

* {
    box-sizing: border-box;
    color: white;
}`,
      bodyClass: "dummy",
      themeColor: undefined,
      children: nHtml.view.childrenText(loadingMessage(urlData.language)),
    },
    isNotFound: false,
  };
};

const getCoverImageUrlAndDescription = async (
  location: d.Location,
  language: d.Language
): Promise<{ imageUrl: URL; description: string; isNotFound: boolean }> => {
  switch (location._) {
    case "Project": {
      const projectResource = await lib.apiFunc.getProject(location.projectId);
      if (projectResource.data._ === "Just") {
        return {
          imageUrl: commonUrl.pngFileUrl(projectResource.data.value.imageHash),
          description:
            projectResource.data.value.name +
            " | Definy で作られたプロジェクト",
          isNotFound: false,
        };
      }
      return {
        imageUrl: commonUrl.iconUrl,
        description: "不明なプロジェクト | Definy",
        isNotFound: true,
      };
    }
    case "Account": {
      const user = await lib.apiFunc.getUser(location.accountId);
      if (user.data._ === "Just") {
        return {
          imageUrl: commonUrl.pngFileUrl(user.data.value.imageHash),
          description: user.data.value.name + " | Definy のアカウント",
          isNotFound: false,
        };
      }
      return {
        imageUrl: commonUrl.iconUrl,
        description: "不明なアカウント | Definy",
        isNotFound: true,
      };
    }
  }
  return {
    imageUrl: commonUrl.iconUrl,
    description: ((): string => {
      switch (language) {
        case "Japanese":
          return "ブラウザで動作する革新的なプログラミング言語!";
        case "Esperanto":
          return "Noviga programlingvo, kiu funkcias en la retumilo";
        case "English":
          return "Definy is Web App for Web App.";
      }
    })(),
    isNotFound: false,
  };
};

const loadingMessage = (language: d.Language): string => {
  switch (language) {
    case "English":
      return "Loading Definy ...";
    case "Japanese":
      return "Definyを読込中……";
    case "Esperanto":
      return "Ŝarĝante Definy ...";
  }
};
