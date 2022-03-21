import { Day } from "common/interfaces/myCalendarInterface";
import * as React from "react";
import { Pressable, StyleSheet, Text } from "react-native";
import { Sizing } from "styles/index";

export interface MonthlyWeekProps extends Day {}

export const MonthlyWeek = ({ number, name }: MonthlyWeekProps) => {
  const [pressed, setPressed] = React.useState(false);

  return (
    <Pressable
      style={styles.dayContainer}
      onPress={() => setPressed((prev) => !prev)}>
      <Text style={[styles.dayNumber, { color: pressed ? "red" : "black" }]}>
        {number}
      </Text>
    </Pressable>
  );
};

const styles = StyleSheet.create({
  dayNumber: {
    padding: Sizing.x5,
    textAlign: "center",
  },
  dayContainer: {
    width: `${100 / 7}%`,
  },
});
