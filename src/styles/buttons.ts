import { TextStyle, ViewStyle, PressableStateCallbackType } from "react-native";

import * as Colors from "./colors";
import * as Outlines from "./outlines";
import * as Sizing from "./sizing";
import * as Typography from "./typography";

type Bar =
  | "primary_light"
  | "primary_dark"
  | "secondary"
  | "transparent_light"
  | "transparent_dark"
  | "small";
export const bar: Record<Bar, ViewStyle> = {
  primary_light: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: Colors.primary.s800,
    paddingVertical: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
    marginTop: Sizing.x15,
    ...Outlines.shadow.lifted,
  },
  primary_dark: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: Colors.primary.neutral,
    paddingVertical: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
    marginTop: Sizing.x15,
    ...Outlines.shadow.lifted,
  },
  secondary: {
    alignItems: "center",
    alignSelf: "center",
    justifyContent: "center",
    padding: Sizing.x12,
    borderRadius: Outlines.borderRadius.base,
    backgroundColor: Colors.secondary.brand,
  },
  transparent_light: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: Colors.primary.neutral,
    paddingVertical: Sizing.x7,
    borderRadius: Outlines.borderRadius.base,
    borderWidth: 3,
    borderColor: Colors.primary.s800,
    marginTop: Sizing.x15,
    ...Outlines.shadow.lifted,
  },
  transparent_dark: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: Colors.primary.s600,
    paddingVertical: Sizing.x7,
    borderRadius: Outlines.borderRadius.base,
    borderWidth: 3,
    borderColor: Colors.primary.neutral,
    marginTop: Sizing.x15,
    ...Outlines.shadow.lifted,
  },
  small: {
    alignItems: "center",
    alignSelf: "center",
    justifyContent: "center",
    padding: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
    backgroundColor: Colors.primary.s200,
  },
};

// text style for each bar (button) type above
type BarText =
  | "primary_light"
  | "primary_dark"
  | "secondary"
  | "transparent_light"
  | "transparent_dark"
  | "small";
export const barText: Record<BarText, TextStyle> = {
  primary_light: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Medium",
    color: Colors.primary.neutral,
  },
  primary_dark: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Medium",
    color: Colors.primary.s800,
  },
  transparent_light: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Bold",
    color: Colors.primary.s800,
  },
  transparent_dark: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Bold",
    color: Colors.primary.neutral,
  },
  small: {
    ...Typography.fontSize.x20,
    ...Typography.fontWeight.semibold,
    color: Colors.neutral.white,
  },
  secondary: {
    ...Typography.fontSize.x10,
    ...Typography.fontWeight.regular,
    color: Colors.neutral.s500,
  },
};

type Circular = "primary";
export const circular: Record<Circular, ViewStyle> = {
  primary: {
    height: Sizing.x30,
    width: Sizing.x30,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: Colors.primary.brand,
    borderRadius: Outlines.borderRadius.max,
  },
};

const opacity = (state: PressableStateCallbackType): ViewStyle => {
  var opacity = state.pressed ? 0.65 : 1;
  return { opacity };
};

export const applyOpacity = (style: ViewStyle | ViewStyle[]) => {
  if ((style as any).length !== undefined)
    style = Object.assign({}, ...(style as any));
  return (state: PressableStateCallbackType): ViewStyle => {
    return {
      ...style,
      ...opacity(state),
    };
  };
};
