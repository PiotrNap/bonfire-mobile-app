import { TextStyle } from "react-native";

import * as Colors from "./colors";
import * as Outlines from "./outlines";
import * as Sizing from "./sizing";
import * as Typography from "./typography";

type Input = "primary" | "primary_light" | "primary_dark";
export const input: Record<Input, TextStyle> = {
  primary: {
    ...Typography.subHeader.x30,
    paddingVertical: Sizing.x12,
    paddingHorizontal: Sizing.x14,
    backgroundColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.thin,
    borderRadius: Outlines.borderRadius.base,
    borderColor: "transparent",
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s600,
    ...Outlines.shadow.lifted,
  },
  primary_light: {
    ...Typography.subHeader.x30,
    paddingVertical: Sizing.x12,
    paddingHorizontal: Sizing.x14,
    backgroundColor: Colors.primary.neutral,
    borderColor: Colors.primary.s800,
    borderWidth: Outlines.borderWidth.thin,
    borderRadius: Outlines.borderRadius.base,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s600,
    ...Outlines.shadow.lifted,
  },
  primary_dark: {
    ...Typography.subHeader.x30,
    paddingVertical: Sizing.x12,
    paddingHorizontal: Sizing.x14,
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    borderWidth: Outlines.borderWidth.thin,
    borderColor: Colors.primary.neutral,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s600,
    ...Outlines.shadow.lifted,
  },
};

type InputLabel = "primary" | "error" | "primary_light" | "primary_dark";
export const inputLabel: Record<InputLabel, TextStyle> = {
  primary: {
    ...Typography.subHeader.x30,
    fontFamily: "Roboto-Medium",
    color: Colors.primary.neutral,
    marginLeft: Sizing.x15,
  },
  primary_light: {
    ...Typography.subHeader.x30,
    fontFamily: "Roboto-Medium",
    color: Colors.primary.s800,
    marginLeft: Sizing.x15,
  },
  primary_dark: {
    ...Typography.subHeader.x30,
    fontFamily: "Roboto-Medium",
    color: Colors.primary.neutral,
    marginLeft: Sizing.x15,
  },
  error: {
    ...Typography.body.x10,
    fontFamily: "Roboto-Medium",
  },
};
