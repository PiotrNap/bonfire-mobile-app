/**
 * @description A wrapper around react native Text component
 * @param children - text node
 * @param colors - an array of two colors for light/dark mode
 */

import * as React from "react";
import { Text, StyleSheet, StyleProp, TextStyle } from "react-native";

import { appContext } from "contexts/contextApi";
import { Typography } from "styles/index";

export interface BodyTextProps {
  children?: React.ReactNode;
  colors: string[];
  customStyle?: StyleProp<TextStyle>;
}

export const BodyText = ({ children, colors, customStyle }: BodyTextProps) => {
  const { colorScheme } = appContext();

  const textColor =
    colorScheme != null && colorScheme === "light"
      ? { color: colors[0] }
      : { color: colors[1] };

  return <Text style={[styles.text, textColor, customStyle]}>{children}</Text>;
};

const styles = StyleSheet.create({
  text: {
    ...Typography.body.x25,
  },
});
