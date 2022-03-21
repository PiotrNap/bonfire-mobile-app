import * as React from "react";
import { Pressable, StyleSheet, Text } from "react-native";
import { Colors, Typography } from "styles/index";

export interface PlaceholderDayProps {
  number: number;
  key: string;
  direction: string;
  onPlaceholderPress: (direction: string) => void;
}

const _PlaceholderDay = ({
  number,
  direction,
  onPlaceholderPress,
}: PlaceholderDayProps) => {
  const onPress = () => {
    onPlaceholderPress(direction);
  };

  return (
    <Pressable onPress={onPress} style={styles.container}>
      <Text style={styles.placeholderNumber}>{number}</Text>
    </Pressable>
  );
};

const styles = StyleSheet.create({
  container: {
    width: `${100 / 7}%`,
    height: `${100 / 6}%`,
    justifyContent: "center",
    alignItems: "center",
  },
  placeholderNumber: {
    ...Typography.body.x30,
    ...Typography.roboto.regular,
    color: Colors.primary.s350,
  },
});

export const PlaceholderDay = React.memo(_PlaceholderDay);
