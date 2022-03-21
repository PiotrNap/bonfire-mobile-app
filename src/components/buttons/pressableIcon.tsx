import * as React from "react";
import { Pressable, StyleSheet } from "react-native";

import { Sizing } from "styles/index";

export interface PressableIconProps {
  onPressCallback: () => void;
  icon: React.ReactNode;
  styles: any;
}

export const PressableIcon = ({
  icon,
  onPressCallback,
  styles,
}: PressableIconProps) => {
  return (
    <Pressable
      hitSlop={5}
      style={[styles, defaultStyles]}
      onPress={onPressCallback}>
      {icon}
    </Pressable>
  );
};

const defaultStyles = StyleSheet.create({
  default: {
    width: "100%",
    height: "100%",
    padding: Sizing.x2,
  },
});
