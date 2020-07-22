/** @jsx jsx */

import * as React from "react";
import * as ui from "./ui";
import { Model } from "./model";
import { jsx } from "react-free-style";

export const CreateProject: React.FC<{ model: Model }> = (prop) => {
  const [projectName, setProjectName] = React.useState("");
  if (prop.model.logInState._ !== "LoggedIn") {
    return (
      <div
        css={{
          padding: 16,
          display: "grid",
          alignContent: "center",
          justifyContent: "center",
        }}
      >
        <div>プロジェクトの作成にはログインする必要があります</div>
        <div>左のログインボタンを押してログインしてください</div>
      </div>
    );
  }

  return (
    <div
      css={{
        padding: 16,
        display: "grid",
        alignContent: "center",
        justifyContent: "center",
      }}
    >
      <div>
        ここはプロジェクト作成ページ.
        プロジェクト名と画像を指定してプロジェクトを作ることができます
      </div>
      <label>
        <div>プロジェクト名</div>
        <input
          onChange={(event) => {
            setProjectName(event.target.value);
          }}
          type="text"
          value={projectName}
        />
      </label>
      <ui.Button
        onClick={() => {
          console.log("プロジェクト作成ボタンを押した");
        }}
      >
        プロジェクトを作成 (作成中)
      </ui.Button>
    </div>
  );
};