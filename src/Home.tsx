/** @jsx jsx */

import * as React from "react";
import * as ui from "./ui";
import {
  Language,
  Location,
  ProjectSnapshot,
  ProjectId,
} from "definy-common/source/data";
import { Model } from "./model";
import { jsx } from "react-free-style";

export const Home: React.FC<{ model: Model }> = (prop) => {
  return (
    <div css={{ display: "grid", overflow: "hidden" }}>
      <div
        css={{
          gridColumn: "1 / 2",
          gridRow: "1 / 2",
          overflow: "hidden",
          overflowWrap: "break-word",
          display: "grid",
          gridTemplateColumns: "1fr 1fr 1fr",
          justifyContent: "center",
        }}
      >
        {prop.model.projectData.size === 0
          ? "プロジェクトが1つもありません"
          : [...prop.model.projectData].map(([id, project]) => {
              switch (project._) {
                case "Loaded":
                  return (
                    <ProjectItem id={id} project={project.snapshot} key={id} />
                  );
                case "Loading":
                  return <div key={id}>id={id}</div>;
                case "NotFound":
                  return (
                    <div key={id}>id={id}のプロジェクトが見つからなかった</div>
                  );
              }
            })}
      </div>
      {prop.model.logInState._ === "Guest" ? undefined : (
        <CreateProjectButton model={prop.model} />
      )}
    </div>
  );
};

const ProjectItem: React.FC<{ id: ProjectId; project: ProjectSnapshot }> = (
  prop
) => (
  <div
    css={{
      padding: 8,
      display: "grid",
      gridTemplateRows: "128px auto",
      maxWidth: 256,
    }}
  >
    <div css={{ border: "solid 1px white" }}>画像</div>
    <div
      css={{
        display: "grid",
        gridTemplateColumns: "32px 1fr",
        gap: 8,
        alignItems: "center",
      }}
    >
      <div css={{ width: 32, height: 32, backgroundColor: "orange" }}>icon</div>
      {prop.project.name}
    </div>
  </div>
);

const CreateProjectButton: React.FC<{ model: Model }> = (prop) => (
  <div
    css={{
      gridColumn: "1 / 2",
      gridRow: "1 / 2",
      alignSelf: "end",
      justifySelf: "end",
      padding: 16,
    }}
  >
    <ui.Link
      areaTheme="Active"
      css={{
        padding: 8,
      }}
      onJump={prop.model.onJump}
      urlData={{ ...prop.model, location: Location.CreateProject }}
    >
      {createProjectMessage(prop.model.language)}
    </ui.Link>
  </div>
);

const createProjectMessage = (language: Language): string => {
  switch (language) {
    case "English":
      return "Create a new project";
    case "Esperanto":
      return "Krei novan projekton";
    case "Japanese":
      return "プロジェクトを新規作成";
  }
};
