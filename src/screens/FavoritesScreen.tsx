import * as React from "react";
import { StyleSheet, Text } from "react-native";

import { SafeAreaView } from "react-native-safe-area-context";

export const FavoritesScreen = ({}) => {
  return (
    <SafeAreaView style={styles.safeArea}>
      <Text>Welcome from Favorites Screen</Text>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
  },
});
