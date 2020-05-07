import { data } from "definy-common";

export namespace Elm {
  namespace Main {
    function init(args: {
      flags: Flags;
      node: HTMLElement;
    }): {
      ports: Ports;
    };
  }
}

type Flags = {
  readonly windowSize: {
    readonly width: number;
    readonly height: number;
  };
  readonly nowTime: data.Time;
  readonly accessTokenMaybe: null | string;
  readonly networkConnection: boolean;
};

type Ports = {
  readonly preventDefaultBeforeKeyEvent: SubForElmCmd<null>;
  readonly consoleLog: SubForElmCmd<string>;
  readonly requestLogInUrl: SubForElmCmd<data.RequestLogInUrlRequestData>;
  readonly getUserByAccessToken: SubForElmCmd<data.AccessToken>;
  readonly getImageBlobUrl: SubForElmCmd<data.ImageToken>;
  readonly createProject: SubForElmCmd<data.CreateProjectParameter>;
  readonly createIdea: SubForElmCmd<data.CreateIdeaParameter>;
  readonly addComment: SubForElmCmd<data.AddCommentParameter>;
  readonly addSuggestion: SubForElmCmd<data.AddSuggestionParameter>;

  readonly toValidProjectName: SubForElmCmd<string>;
  readonly toValidIdeaName: SubForElmCmd<string>;

  /**
   * indexedDBにあるキャッシュを使用してユーザーを取得する. indexedDBになければサーバーから取得する. 取得したユーザーはindexedDBに保存する
   */
  readonly getUser: SubForElmCmd<data.UserId>;
  /**
   * indexedDBから読み取らずにユーザーをサーバーから取得する. 取得したユーザーはindexedDBに保存する
   */
  readonly getUserNoCache: SubForElmCmd<data.UserId>;

  readonly getAllProjectIdList: SubForElmCmd<null>;
  /**
   * indexedDBにあるキャッシュを使用してプロジェクトを取得する. indexedDBになければサーバーから取得する. 取得したプロジェクトはindexedDBに保存する
   */
  readonly getProject: SubForElmCmd<data.ProjectId>;
  /**
   * indexedDBから読み取らずにプロジェクトをサーバーから取得する. 取得したプロジェクトはindexedDBに保存する
   */
  readonly getProjectNoCache: SubForElmCmd<data.ProjectId>;
  readonly getIdeaAndIdListByProjectId: SubForElmCmd<data.ProjectId>;

  /**
   * indexedDBにあるキャッシュを使用してアイデアを取得する. indexedDBになければサーバーから取得する. 取得したアイデアはindexedDBに保存する
   */
  readonly getIdea: SubForElmCmd<data.IdeaId>;
  /**
   * indexedDBから読み取らずにアイデアをサーバーから取得する. 取得したアイデアはindexedDBに保存する
   */
  readonly getIdeaNoCache: SubForElmCmd<data.IdeaId>;

  readonly getSuggestion: SubForElmCmd<data.SuggestionId>;
  readonly getSuggestionNoCache: SubForElmCmd<data.SuggestionId>;

  readonly responseUserByAccessToken: CmdForElmSub<
    data.Maybe<data.UserSnapshotAndId>
  >;
  readonly keyPressed: CmdForElmSub<KeyboardEvent>;
  readonly keyPrevented: CmdForElmSub<null>;
  readonly windowResize: CmdForElmSub<{
    width: number;
    height: number;
  }>;
  readonly changeNetworkConnection: CmdForElmSub<boolean>;
  readonly subPointerUp: CmdForElmSub<null>;
  readonly getImageBlobResponse: CmdForElmSub<{
    blobUrl: string;
    imageToken: string;
  }>;
  readonly toValidProjectNameResponse: CmdForElmSub<{
    input: string;
    result: string | null;
  }>;
  readonly toValidIdeaNameResponse: CmdForElmSub<{
    input: string;
    result: string | null;
  }>;
  readonly createProjectResponse: CmdForElmSub<
    data.Maybe<data.ProjectSnapshotAndId>
  >;
  readonly responseCreateIdea: CmdForElmSub<data.Maybe<data.IdeaSnapshotAndId>>;
  readonly responseAllProjectId: CmdForElmSub<ReadonlyArray<data.ProjectId>>;
  readonly responseProject: CmdForElmSub<data.ProjectResponse>;
  readonly responseUser: CmdForElmSub<data.UserResponse>;
  readonly responseAddComment: CmdForElmSub<data.IdeaResponse>;
  readonly responseAddSuggestion: CmdForElmSub<
    data.Maybe<data.SuggestionSnapshotAndId>
  >;
  readonly responseIdeaSnapshotAndIdListByProjectId: CmdForElmSub<{
    projectId: data.ProjectId;
    ideaSnapshotAndIdList: ReadonlyArray<data.IdeaSnapshotAndId>;
  }>;
  readonly responseIdea: CmdForElmSub<data.IdeaResponse>;
  readonly responseSuggestion: CmdForElmSub<data.SuggestionResponse>;
};

type SubForElmCmd<T> = {
  subscribe: (arg: (value: T) => void) => void;
};

type CmdForElmSub<T> = {
  send: (value: T) => void;
};
