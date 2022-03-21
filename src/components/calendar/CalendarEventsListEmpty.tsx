import * as React from "react";
import { View, Text, StyleSheet } from "react-native";

import { Typography, Colors } from "styles/index";

export const CalendarEventsListEmpty = () => {
  return (
    <View style={styles.container}>
      <Text style={styles.text}>Nothing here yet...</Text>
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    height: "25%",
    alignItems: "center",
    justifyContent: "center",
  },
  text: {
    ...Typography.subHeader.x30,
  },
});
