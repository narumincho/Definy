import { SelectBoxSelection, box, grayText, selectBox } from "../ui";
import { Element } from "@narumincho/html/view";
import { c } from "@narumincho/html/viewUtil";

export type ProductItem<Message> = {
  readonly name: string;
  readonly element: Element<Message>;
};

export const productEditor = <Message>(
  option: {
    selectBoxOption?: {
      selection: SelectBoxSelection;
      selectMessage: Message;
    };
  },
  itemList: ReadonlyArray<ProductItem<Message>>
): Element<Message> => {
  const children = new Map(
    itemList.map((item): readonly [string, Element<Message>] => [
      item.name,
      box(
        {
          padding: 4,
          direction: "y",
          borderRadius: 8,
        },
        c([
          ["name", grayText(item.name)],
          ["value", item.element],
        ])
      ),
    ])
  );
  if (option.selectBoxOption === undefined) {
    return box(
      {
        direction: "y",
        padding: 8,
      },
      children
    );
  }
  return selectBox(
    {
      direction: "y",
      padding: 8,
      selectMessage: option.selectBoxOption.selectMessage,
      selection: option.selectBoxOption.selection,
    },
    children
  );
};
