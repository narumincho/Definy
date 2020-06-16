import * as codeGen from "js-ts-code-generator";
import * as nType from "@narumincho/type";
import * as prettier from "prettier";
import {
  CustomTypeDefinition,
  CustomTypeDefinitionBody,
  Maybe,
  Type,
} from "@narumincho/type/distribution/data";
import { promises as fileSystem } from "fs";

export const customTypeList: ReadonlyArray<CustomTypeDefinition> = [
  {
    name: "Resource",
    description: "ProjectやUserなどのリソースの保存状態を表す",
    typeParameterList: ["data"],
    body: CustomTypeDefinitionBody.Sum([
      {
        name: "Loading",
        description:
          "サーバーにリクエストしている最中や, indexedDBから読み込んでいるとき",
        parameter: Maybe.Nothing(),
      },
      {
        name: "Loaded",
        description: "データを取得済み",
        parameter: Maybe.Just(Type.Parameter("data")),
      },
      {
        name: "NotFound",
        description: "データが見つからなかった",
        parameter: Maybe.Nothing(),
      },
    ]),
  },
];

const code = prettier.format(
  codeGen.generateCodeAsString(
    nType.generateTypeScriptCode(customTypeList),
    "TypeScript"
  ),
  { parser: "typescript" }
);

const outFilePath = "./src/data.ts";

fileSystem.writeFile(outFilePath, code).then(() => {
  console.log(`output complete. ${outFilePath}`);
});
