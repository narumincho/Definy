import * as core from "definy-core";
import * as d from "definy-core/source/data";
import react, { Component, ReactNode, createElement as h } from "react";
import { Model } from "./model";
import { Theme } from "./ui";
import styled from "styled-components";

export type Props = {
  readonly model: Model;
  readonly location: d.Location;
  readonly language?: d.Language;
  readonly areaTheme: Theme;
  readonly className?: string;
};

export class Link extends Component<Props, never> {
  onClick(event: react.MouseEvent<HTMLAnchorElement, MouseEvent>): void {
    /*
     * リンクを
     * Ctrlなどを押しながらクリックか,
     * マウスの中ボタンでクリックした場合などは, ブラウザで新しいタブが開くので, ページ推移をDefinyでしない.
     */
    if (
      event.ctrlKey ||
      event.metaKey ||
      event.shiftKey ||
      event.button !== 0
    ) {
      return;
    }
    event.preventDefault();
    this.props.model.jump(this.props.location, this.props.model.language);
  }

  render(): ReactNode {
    return h(
      Link_,
      {
        href: core
          .urlDataAndAccountTokenToUrl(
            {
              clientMode: this.props.model.clientMode,
              language: this.props.language ?? this.props.model.language,
              location: this.props.location,
            },
            d.Maybe.Nothing()
          )
          .toString(),
        onClick: this.onClick,
        className: this.props.className,
      },
      this.props.children
    );
  }
}

const themeToColor = (props: Props): string => {
  switch (props.areaTheme) {
    case "Gray":
      return "#ddd";
    case "Black":
      return "#ddd";
    case "Active":
      return "#000";
  }
};

const Link_ = styled.a`
  display: block;
  text-decoration: none;
  color: ${themeToColor};
`;

const themeToStyle = (
  theme: Theme
): {
  color: string;
  backgroundColor: string;
  "&:hover": {
    backgroundColor: string;
    color: string;
  };
} => {
  switch (theme) {
    case "Gray":
      return {
        backgroundColor: "#333",
        color: "#ddd",
        "&:hover": {
          backgroundColor: "#444",
          color: "#dfdfdf",
        },
      };
    case "Black":
      return {
        backgroundColor: "#000",
        color: "#ddd",
        "&:hover": {
          backgroundColor: "#111",
          color: "#dfdfdf",
        },
      };
    case "Active":
      return {
        backgroundColor: "#f0932b",
        color: "#000",
        "&:hover": {
          backgroundColor: "#f69d3a",
          color: "#000",
        },
      };
  }
};
