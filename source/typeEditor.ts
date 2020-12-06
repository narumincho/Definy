import * as d from "definy-core/source/data";
import { Editor, editorToReactElement, mapEditor } from "./ui";
import { Model } from "./model";
import { createProductEditor } from "./productEditor";
import { createWithParameterSumEditor } from "./sumEditor";
import { jsx as h } from "@emotion/react";

export const TypeEditor: Editor<d.Type> = (props) => {
  const typePartList = getTypePartLoaded(props.model);
  const typeProductEditor = createWithParameterSumEditor<
    Record<string, ReadonlyArray<d.Type>>,
    string,
    { _: string; param: ReadonlyArray<d.Type> }
  >(
    Object.fromEntries(
      typePartList.map((idAndData): [string, Editor<ReadonlyArray<d.Type>>] => [
        idAndData.data.name,
        typeParameterListEditor(
          idAndData.data.typeParameterList,
          idAndData.data.name
        ),
      ])
    ),
    Object.fromEntries(
      typePartList.map((idAndData): [
        string,
        { _: string; param: ReadonlyArray<d.Type> }
      ] => [
        idAndData.data.name,
        {
          _: idAndData.data.name,
          param: new Array<d.Type>(
            idAndData.data.typeParameterList.length
          ).fill({
            typePartId: "15585b6605524aea7b86e0803ad95163" as d.TypePartId,
            parameter: [],
          }),
        },
      ])
    ),
    "TypeEditor"
  );

  return editorToReactElement(
    mapEditor<{ _: string; param: ReadonlyArray<d.Type> }, d.Type>(
      typeProductEditor,
      (record): d.Type => ({
        typePartId: typePartList.find(
          (typePart) => typePart.data.name === record._
        )?.id as d.TypePartId,
        parameter: record.param,
      }),
      (type: d.Type) => ({
        _: typePartList.find((typePart) => typePart.id === type.typePartId)
          ?.data.name as string,
        param: type.parameter,
      })
    ),
    {
      model: props.model,
      onChange: props.onChange,
      value: props.value,
      name: props.name,
    }
  );
};

const typeParameterListEditor = (
  typeParameterList: ReadonlyArray<d.TypeParameter>,
  typeName: string
): Editor<ReadonlyArray<d.Type>> => {
  if (typeParameterList.length === 0) {
    return EmptyTypeParameterListEditor;
  }
  if (typeParameterList.length === 1) {
    return mapEditor<d.Type, ReadonlyArray<d.Type>>(
      TypeEditor,
      (type) => [type],
      (typeList) => typeList[0]
    );
  }
  return mapEditor<Record<string, d.Type>, ReadonlyArray<d.Type>>(
    createProductEditor(
      Object.fromEntries(
        typeParameterList.map((typeParameter) => [
          typeParameter.name,
          TypeEditor,
        ])
      ),
      typeName + "-typeParameterEditor"
    ),
    (record: Record<string, d.Type>) =>
      typeParameterList.map((typeParameter) => record[typeParameter.name]),
    (list: ReadonlyArray<d.Type>) =>
      Object.fromEntries(
        list.map((type, index) => [typeParameterList[index].name, type])
      )
  );
};

const EmptyTypeParameterListEditor: Editor<ReadonlyArray<d.Type>> = () =>
  h("div", {});

const getTypePartLoaded = (
  model: Model
): ReadonlyArray<d.IdAndData<d.TypePartId, d.TypePart>> =>
  [...model.typePartMap].flatMap(([id, typePart]) => {
    if (typePart._ !== "Loaded") {
      return [];
    }
    return [{ id, data: typePart.dataWithTime.data }];
  });
