import * as React from "react";
import { Text, StyleSheet, StyleProp, TextStyle } from "react-native";
import { Colors, Typography } from "styles/index";

export interface HeaderTextProps {
  colorScheme: "light" | "dark";
  children: React.ReactNode;
  customStyles?: StyleProp<TextStyle>;
}

export const HeaderText = ({
  colorScheme,
  children,
  customStyles,
}: HeaderTextProps) => {
  const textStyle =
    colorScheme === "light" ? styles.text_light : styles.text_dark;

  return <Text style={[textStyle, customStyles]}>{children}</Text>;
};

const styles = StyleSheet.create({
  text_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  text_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
});
