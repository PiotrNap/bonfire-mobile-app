import * as React from "react";
import { FallbackProps } from "react-error-boundary";
import { Text, Pressable, View, StyleSheet } from "react-native";
import { SafeAreaView } from 'react-native-safe-area-context'


export const CalendarError = ({ error, resetErrorBoundary }: FallbackProps) => {
  return (
    <SafeAreaView style={styles.safeArea}>
      <View style={styles.container}>
        <Text>Something went wrong:</Text>
        <Text>{error.toString()}</Text>
        <Pressable onPress={resetErrorBoundary}>
          <Text>Try again</Text>
        </Pressable>
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
  },
  container: {
    alignItems: "center",
    justifyContent: "center",
  },
});
