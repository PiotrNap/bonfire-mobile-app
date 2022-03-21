import { ViewStyle, TextStyle } from "react-native";

import * as Colors from "./colors";
import * as Sizing from "./sizing";
import * as Outlines from "./outlines";
import * as Typography from "./typography";

type TagContainer = "small" | "medium" | "large";
export const tagContainer: Record<TagContainer, ViewStyle> = {
  small: {
    borderRadius: Outlines.borderRadius.large,
  },
  large: {},
  medium: {},
};

type TagHeader = "small" | "medium" | "large";
export const tagHeader: Record<TagHeader, TextStyle> = {
  small: {
    ...Typography.header.x10,
    fontSize: Sizing.x10,
  },
  large: {},
  medium: {},
};
