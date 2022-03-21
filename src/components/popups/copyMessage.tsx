import * as React from "react";
import { Text, StyleSheet, Animated } from "react-native";
import { Colors, Outlines, Sizing, Typography } from "styles/index";

export interface CopyMessageProps {
  isActive: boolean;
}

export const CopyMessage = ({ isActive }: CopyMessageProps) => {
  const copyMsgPosition = React.useRef(new Animated.ValueXY()).current;
  const copyMsgOpacity = React.useRef(new Animated.Value(0)).current;

  React.useEffect(() => {
    Animated.parallel([
      Animated.timing(copyMsgOpacity, {
        useNativeDriver: false,
        toValue: isActive ? 1 : 0,
        duration: 80,
      }),
      Animated.timing(copyMsgPosition.x, {
        useNativeDriver: false,
        toValue: isActive ? 30 : 0,
        duration: 80,
      }),
      Animated.timing(copyMsgPosition.y, {
        useNativeDriver: false,
        toValue: isActive ? -25 : 0,
        duration: 80,
      }),
    ]).start();
  }, [isActive]);

  return (
    <Animated.View
      style={[
        styles.container,
        {
          opacity: copyMsgOpacity,
          top: copyMsgPosition.y,
          right: copyMsgPosition.x,
        },
      ]}>
      <Text style={styles.text}>Copied!</Text>
    </Animated.View>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 1,
    position: "absolute",
    backgroundColor: Colors.neutral.s400,
    paddingVertical: Sizing.x2,
    paddingHorizontal: Sizing.x8,
    borderRadius: Outlines.borderRadius.small,
  },
  text: {
    ...Typography.subHeader.x25,
    color: Colors.primary.neutral,
  },
});
