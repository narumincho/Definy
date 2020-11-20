import styled, { StyledComponent } from "styled-components";
import { ReactElement } from "react";

export type GridTemplate = { _: "Fix"; value: number } | { _: "OneFr" };

export type SimpleStyle = {
  /** 方向 x → 横方向, y → 縦方向 */
  direction: "x" | "y";
  /** 幅 */
  width?: number;
  /** 高さ */
  height?: number;
  /** 間の余白 */
  gap?: number;
  /** 余白 */
  padding: number;
  /** 丸角 デフォルト 0 */
  borderRadius?: number;
  /** ボーダー */
  border?: { width: number; color: string };
  /** 横方向を均等に区切る. 横の個数 */
  ySameCellCount?: number;
};

/** CSSの指定をできるだけしなくて済むように */
export const styledDiv = (
  simpleStyle: SimpleStyle
): StyledComponent<
  "div",
  Record<string, unknown>,
  Record<never, never>,
  never
> =>
  styled.div({
    display: "grid",
    boxSizing: "border-box",
    wordBreak: "break-all",
    breakWord: "break-word",
    gridAutoFlow: simpleStyle.direction === "x" ? "column" : "row",
    width: simpleStyle.width,
    height: simpleStyle.height,
    gap: simpleStyle.gap,
    padding: simpleStyle.padding,
    backgroundColor: "#111",
    color: "#ddd",
    borderRadius: simpleStyle.borderRadius,
    border:
      simpleStyle.border === undefined
        ? "none"
        : "solid " +
          simpleStyle.border.width.toString() +
          "px " +
          simpleStyle.border.color,
    gridTemplateColumns:
      simpleStyle.ySameCellCount === undefined
        ? undefined
        : Array.from({ length: simpleStyle.ySameCellCount }, () => "1fr").join(
            " "
          ),
  });

export type Editor<T> = (props: {
  value: T;
  onChange: (newValue: T) => void;
  name: string;
}) => ReactElement;
