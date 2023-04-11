import { StyleSheet, TextStyle } from "react-native"

import * as Colors from "./colors"
import * as Outlines from "./outlines"
import * as Sizing from "./sizing"
import * as Typography from "./typography"

type Input = "primary" | "primary_light" | "primary_dark"
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
}

type InputLabel = "primary" | "error" | "primary_light" | "primary_dark"
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
    color: Colors.primary.s200,
    marginLeft: Sizing.x15,
  },
  error: {
    ...Typography.body.x10,
    fontFamily: "Roboto-Medium",
  },
}

export const formStyleLight = StyleSheet.create({
  label: {
    ...inputLabel.primary_light,
  },
  input: {
    width: "100%",
    ...input.primary_light,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s600,
    ...Typography.subHeader.x30,
    marginRight: "auto",
  },
})

export const formStyleDark = StyleSheet.create({
  label: {
    ...inputLabel.primary_dark,
  },
  input: {
    width: "100%",
    ...input.primary_dark,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s600,
    ...Typography.subHeader.x30,
    marginRight: "auto",
  },
})

/**
 * Styles passed as props to CustomInput
 */
export const inputStyles = StyleSheet.create({
  inputContainer: {
    width: "100%",
    alignItems: "center",
  },
  labelContainer: {
    width: "100%",
  },
  label: {
    ...inputLabel.primary,
  },
  textInputWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
  },
  input: {
    width: "100%",
    ...input.primary,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
  errorInput: {
    borderColor: Colors.danger.s300,
  },
  errorWrapper: {
    justifyContent: "center",
    alignItems: "center",
    alignSelf: "center",
    height: Sizing.x20,
  },
  error: {
    color: Colors.danger.s400,
    ...Typography.header.x20,
  },
})
