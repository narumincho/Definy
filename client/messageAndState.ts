import * as d from "../data";
import type * as pageCreateProject from "./page/createProject";
import * as pageDebug from "./page/debug";
import type * as pageProject from "./page/project";
import { Element } from "@narumincho/html/view";
import type { PageState } from "./page";
import type { Message as TypePartEditorMessage } from "./ui/typePartEditor";

export type HomeProjectState =
  | { _: "None" }
  | { _: "Loading" }
  | { _: "Loaded"; projectIdList: ReadonlyArray<d.ProjectId> };

export type TypePartEditSate = "None" | "Adding" | "Saving" | "Error";

export type GetTypePartInProjectState =
  | { _: "None" }
  | { _: "Requesting"; projectId: d.ProjectId };

export type RequestTypePartListInProjectState =
  | { _: "None" }
  | { _: "WaitRequesting"; projectId: d.ProjectId }
  | { _: "Requesting"; projectId: d.ProjectId };

export type State = {
  /** ホームに表示される. Top50のプロジェクトのID */
  readonly top50ProjectIdState: HomeProjectState;

  /** プロジェクトの辞書 */
  readonly projectMap: ReadonlyMap<d.ProjectId, d.ResourceState<d.Project>>;

  /** ユーザーの辞書 */
  readonly userMap: ReadonlyMap<d.AccountId, d.ResourceState<d.Account>>;

  /** 型パーツの辞書 */
  readonly typePartMap: ReadonlyMap<d.TypePartId, d.ResourceState<d.TypePart>>;

  /** プロジェクト作成中かどうか */
  readonly isCreatingProject: boolean;

  /** 型パーツ編集状態 */
  readonly typePartEditState: TypePartEditSate;

  /** プロジェクトに属する型パーツの取得状態 */
  readonly getTypePartInProjectState: GetTypePartInProjectState;

  /** ページの言語 */
  readonly language: d.Language;

  /** ログイン状態 */
  readonly logInState: d.LogInState;

  /** 出力されたコード */
  readonly outputCode: OutputCode;

  /**どこのページを開いているかとそのページの状態 */
  readonly pageState: PageState;

  /** 型を検索するためのキーワード */
  readonly typeSearchText: string;
};

export type OutputCode =
  | {
      readonly tag: "notGenerated";
    }
  | {
      readonly tag: "generated";
      readonly typeScript: string;
      readonly javaScript: string;
      readonly elm: string;
    }
  | {
      readonly tag: "error";
      readonly errorMessage: string;
    };

/**
 * アカウントトークンを得る
 */
export const getAccountToken = (state: State): d.AccountToken | undefined => {
  switch (state.logInState._) {
    case "LoggedIn":
      return state.logInState.accountTokenAndUserId.accountToken;
  }
};

export type Message =
  | { readonly tag: typeof messageNoOp }
  | {
      readonly tag: typeof messageJumpTag;
      readonly location: d.Location;
      readonly language: d.Language;
    }
  | {
      readonly tag: typeof messageChangeLocationAndLanguage;
      readonly location: d.Location;
      readonly language: d.Language;
    }
  | {
      readonly tag: typeof messageRequestLogInTag;
      readonly provider: d.OpenIdConnectProvider;
    }
  | {
      readonly tag: typeof messageRespondLogInUrlTag;
      readonly logInUrlMaybe: d.Maybe<string>;
    }
  | {
      readonly tag: typeof messageLogOut;
    }
  | {
      readonly tag: typeof messageGetUserTag;
      readonly userId: d.AccountId;
    }
  | {
      readonly tag: typeof messageRespondUserTag;
      readonly userId: d.AccountId;
      readonly response: d.Maybe<d.WithTime<d.Maybe<d.Account>>>;
    }
  | {
      readonly tag: typeof messageRespondAccountTokenFromIndexedDB;
      readonly accountToken: d.AccountToken | undefined;
    }
  | {
      readonly tag: typeof messageRespondUserByAccountToken;
      readonly response: d.Maybe<d.Maybe<d.IdAndData<d.AccountId, d.Account>>>;
    }
  | {
      readonly tag: typeof messageGetTop50Project;
    }
  | {
      readonly tag: typeof messageRespondAllTop50Project;
      readonly response: d.Maybe<
        d.WithTime<ReadonlyArray<d.IdAndData<d.ProjectId, d.Project>>>
      >;
    }
  | {
      readonly tag: typeof messageGetProject;
      readonly projectId: d.ProjectId;
    }
  | {
      readonly tag: typeof messageRespondProject;
      readonly projectId: d.ProjectId;
      readonly response: d.Maybe<d.WithTime<d.Maybe<d.Project>>>;
    }
  | {
      readonly tag: typeof messageGenerateCode;
    }
  | {
      readonly tag: typeof messageGetTypePartInProject;
      readonly projectId: d.ProjectId;
    }
  | {
      readonly tag: typeof messageRespondTypePartInProject;
      readonly response: d.Maybe<
        d.WithTime<d.Maybe<d.List<d.IdAndData<d.TypePartId, d.TypePart>>>>
      >;
    }
  | {
      readonly tag: typeof messageCreateProject;
      readonly projectName: string;
    }
  | {
      readonly tag: typeof messageRespondCreatingProject;
      readonly response: d.Maybe<d.Maybe<d.IdAndData<d.ProjectId, d.Project>>>;
    }
  | {
      readonly tag: typeof messageSetTypePartList;
      readonly projectId: d.ProjectId;
    }
  | {
      readonly tag: typeof messageRespondTypePartList;
      readonly response: d.Maybe<
        d.WithTime<d.Maybe<d.List<d.IdAndData<d.TypePartId, d.TypePart>>>>
      >;
    }
  | {
      readonly tag: typeof messageSelectDebugPageTab;
      readonly tab: pageDebug.Tab;
    }
  | {
      readonly tag: typeof messageTypePartMessage;
      readonly typePartId: d.TypePartId;
      readonly typePartMessage: TypePartEditorMessage;
    }
  | {
      readonly tag: "AddTypePart";
      readonly projectId: d.ProjectId;
    }
  | {
      readonly tag: "PageProject";
      readonly message: pageProject.PageMessage;
    }
  | {
      readonly tag: "SaveTypePart";
      readonly typePartId: d.TypePartId;
    }
  | {
      readonly tag: "RespondSavingTypePart";
      readonly typePartId: d.TypePartId;
      readonly response: d.Maybe<d.WithTime<d.Maybe<d.TypePart>>>;
    }
  | {
      readonly tag: "AddTypePartNoSave";
      readonly projectId: d.ProjectId;
    }
  | {
      readonly tag: "RespondAddTypePart";
      readonly response: d.Maybe<
        d.WithTime<d.Maybe<d.IdAndData<d.TypePartId, d.TypePart>>>
      >;
    }
  | {
      readonly tag: "SetTypeSearchText";
      readonly text: string;
    }
  | {
      readonly tag: "CreateProjectPageMessage";
      readonly message: pageCreateProject.Message;
    };

export const messageNoOp = Symbol("Message-NoOp");
export const messageJumpTag = Symbol("Message-Jump");
export const messageChangeLocationAndLanguage = Symbol(
  "Message-ChangeLocationAndLanguage"
);
export const messageRequestLogInTag = Symbol("Message-RequestLogIn");
export const messageRespondLogInUrlTag = Symbol("Message-RespondLogInUrlTag");
export const messageLogOut = Symbol("Message-LogOut");
export const messageGetUserTag = Symbol("Message-GetUser");
export const messageRespondUserTag = Symbol("Message-RespondUser");
export const messageRespondAccountTokenFromIndexedDB = Symbol(
  "Message-RespondAccountTokenFromIndexedDB"
);
export const messageRespondUserByAccountToken = Symbol(
  "Message-RespondUserByAccountToken"
);
export const messageGetTop50Project = Symbol("Message-GetTop50Project");
export const messageRespondAllTop50Project = Symbol(
  "Message-RespondAllTop50Project"
);
export const messageGetProject = Symbol("Message-GetProject");
export const messageRespondProject = Symbol("Message-RespondProject");
export const messageGenerateCode = Symbol("Message-GenerateCode");
export const messageGetTypePartInProject = Symbol(
  "Message-GetTypePartInProject"
);
export const messageRespondTypePartInProject = Symbol(
  "Message-RespondTypePartInProject"
);
export const messageCreateProject = Symbol("Message-CreateProject");
export const messageRespondCreatingProject = Symbol(
  "Message-RespondCreatingProject"
);
export const messageSetTypePartList = Symbol("Message-SetTypePartList");
export const messageRespondTypePartList = Symbol(
  "Message-RespondSetTypePartList"
);
export const messageSelectDebugPageTab = Symbol("Message-SelectDebugPageTab");
export const messageTypePartMessage = Symbol("Message-TypePartMessage");

export type TitleAndElement<M = Message> = {
  readonly title: string;
  readonly element: Element<M>;
};

export type TypePartBodyTag = "Product" | "Sum" | "Kernel";
