import * as React from "react";

import * as Updates from "expo-updates";
import { Pressable, View, Text, StyleSheet } from "react-native";
import { SafeAreaView } from "react-native-safe-area-context";
import { ErrorBoundary, FallbackProps } from "react-error-boundary";
import { Buttons, Typography } from "styles/index";

export interface ErrorHandlerProps {
  children: React.ReactNode;
}

export const myErrorHandler = (error: Error) => {
  //@TODO: do something with the error
  return error;
};

export const ErrorFallback = ({ error, resetErrorBoundary }: FallbackProps) => {
  return (
    <SafeAreaView style={{ flex: 1, alignItems: "center" }}>
      <View style={styles.container}>
        <Text style={[Typography.header.x40, { textAlign: "center" }]}>
          Hey gimbal, something went wrong:
        </Text>
        <Text style={[Typography.body.x20, { textAlign: "center" }]}>
          {error.message}
        </Text>
        <Pressable
          onPress={() => resetErrorBoundary()}
          style={Buttons.applyOpacity(styles.button)}>
          <Text style={Buttons.barText.primary_light}>Try again</Text>
        </Pressable>
        <Pressable
          onPress={async () => await Updates.reloadAsync()}
          style={Buttons.applyOpacity(styles.button)}>
          <Text style={Buttons.barText.primary_light}>Restart</Text>
        </Pressable>
      </View>
    </SafeAreaView>
  );
};

export const ErrorHandler = ({ children }: ErrorHandlerProps) => {
  return (
    <ErrorBoundary FallbackComponent={ErrorFallback} onError={myErrorHandler}>
      {children}
    </ErrorBoundary>
  );
};

const styles = StyleSheet.create({
  container: {
    width: "80%",
    alignItems: "center",
    justifyContent: "center",
    flex: 1,
  },
  button: {
    ...Buttons.bar.primary_light,
  },
});
