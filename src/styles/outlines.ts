import { Platform, StyleSheet, ViewStyle } from "react-native";
import * as Colors from "./colors";

type BorderWidth = "hairline" | "thin" | "base" | "thick";
export const borderWidth: Record<BorderWidth, number> = {
  hairline: StyleSheet.hairlineWidth,
  thin: 1,
  base: 2,
  thick: 3,
};

type BorderRadius = "small" | "base" | "large" | "max";
export const borderRadius: Record<BorderRadius, number> = {
  small: 5,
  base: 10,
  large: 20,
  max: 9999,
};

type Shadow = "base" | "lifted" | "lifted_noElevation";
export const shadow: Record<Shadow, ViewStyle> = {
  base: {
    shadowColor: Colors.neutral.s400,
    shadowOffset: {
      width: 0,
      height: 2,
    },
    shadowOpacity: 0.27,
    shadowRadius: 2.65,
  },
  lifted: {
    shadowColor: Colors.neutral.s600,
    shadowOffset: {
      width: 0,
      height: 4,
    },
    shadowOpacity: 0.4,
    shadowRadius: 3,
    elevation: Platform.OS === "android" ? 4 : 0,
  },
  lifted_noElevation: {
    shadowColor: Colors.neutral.s600,
    shadowOffset: {
      width: 0,
      height: 4,
    },
    shadowOpacity: 0.4,
    shadowRadius: 3,
  },
};
