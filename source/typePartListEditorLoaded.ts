import * as d from "definy-core/source/data";
import * as definyCoreSchema from "definy-core/schema/typePartMap";
import { Component, ReactElement } from "react";
import { css, jsx } from "@emotion/react";
import { Button } from "./button";
import { Editor } from "./ui";
import { Model } from "./model";
import { OneLineTextInput } from "./oneLineTextInput";
import { ProjectIdEditor } from "./projectIdEditor";
import { TypePartBodyEditor } from "./typePartBodyEditor";
import { createListEditor } from "./listEditor";
import { createMaybeEditor } from "./maybeEditor";
import { createNoParameterTagEditor } from "./sumEditor";
import { createProductEditor } from "./productEditor";

export type Props = {
  readonly initTypePartList: ReadonlyMap<d.TypePartId, d.TypePart>;
  readonly projectId: d.ProjectId;
  readonly model: Model;
};

type State = {
  readonly typePartList: ReadonlyMap<d.TypePartId, d.TypePart>;
};

export class TypePartListEditorLoaded extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      typePartList: props.initTypePartList,
    };
  }

  addTypePart(): void {
    this.props.model.addTypePart(this.props.projectId);
  }

  setAt(typePartId: d.TypePartId, typePart: d.TypePart): void {
    this.setState({
      typePartList: new Map(this.state.typePartList).set(typePartId, typePart),
    });
  }

  save(): void {
    this.props.model.setTypePartList(
      this.props.projectId,
      [...this.state.typePartList].map(([id, data]) => ({
        id,
        data,
      }))
    );
  }

  setDefinyCodeCode(): void {
    this.setState({
      typePartList: definyCoreSchema.typePartMap,
    });
  }

  render(): ReactElement {
    return jsx(
      "div",
      {
        css: css({
          display: "grid",
          gap: 8,
        }),
      },
      [...this.state.typePartList].map(([id, data]) =>
        jsx(
          "div",
          { key: id },
          jsx("div", { key: "id" }, id),
          jsx(TypePartEditor, {
            key: "data",
            name: "typePart-" + id,
            value: data,
            onChange: (newTypePart: d.TypePart) => {
              this.setAt(id, newTypePart);
            },
            model: this.props.model,
          })
        )
      ),
      jsx(
        Button,
        {
          onClick: () => this.addTypePart(),
          key: "typePartAddButton",
        },
        "型パーツ追加"
      ),
      jsx(
        Button,
        {
          onClick: () => this.save(),
          key: "save",
        },
        "保存"
      ),
      jsx(
        Button,
        {
          onClick: () => this.props.model.generateCode(this.state.typePartList),
          key: "generateButton",
        },
        "TypeScriptのコードを生成する"
      ),
      jsx(
        Button,
        {
          onClick: () => this.setDefinyCodeCode(),
          key: "setDefinyCoreSchema",
        },
        "definy-coreのスキーマを設定する"
      ),
      jsx("textarea", {
        key: "outputCode",
        value:
          this.props.model.outputCode === undefined
            ? "コード生成中か,まだ生成していない"
            : this.props.model.outputCode,
        rows: 10,
        readOnly: true,
      })
    );
  }
}

const AttributeEditor: Editor<d.TypeAttribute> = createNoParameterTagEditor<d.TypeAttribute>(
  [d.TypeAttribute.AsBoolean, d.TypeAttribute.AsUndefined]
);

const AttributeMaybeEditor: Editor<
  d.Maybe<d.TypeAttribute>
> = createMaybeEditor<d.TypeAttribute>(
  AttributeEditor,
  d.TypeAttribute.AsBoolean
);

const TypeParameterListEditor: Editor<
  ReadonlyArray<d.TypeParameter>
> = createListEditor<d.TypeParameter>({
  isLazy: false,
  editor: createProductEditor<d.TypeParameter>(
    {
      name: OneLineTextInput,
      typePartId: (OneLineTextInput as unknown) as Editor<d.TypePartId>,
    },
    "TypeParameter"
  ),
  initValue: {
    name: "initTypeParameterValue",
    typePartId: "15585b6605524aea7b86e0803ad95163" as d.TypePartId,
  },
  displayName: "TypeParameterListEditor",
});

const TypePartEditor: Editor<d.TypePart> = createProductEditor<d.TypePart>(
  {
    name: OneLineTextInput,
    description: OneLineTextInput,
    attribute: AttributeMaybeEditor,
    projectId: ProjectIdEditor,
    typeParameterList: TypeParameterListEditor,
    body: TypePartBodyEditor,
  },
  "TypePartEditor"
);
