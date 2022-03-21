import * as React from "react";
import { StyleSheet } from "react-native";

import { appContext } from "contexts/contextApi";
import { MyCalendarProvider } from "contexts/myCalendarContext";
import { SafeAreaView } from "react-native-safe-area-context";
import { Colors } from "styles/index";

export interface CalendarWrapperProps {
  children: React.ReactNode;
}

export const CalendarWrapper = ({ children }: CalendarWrapperProps) => {
  const { colorScheme } = appContext();
  return (
    <SafeAreaView
      style={
        colorScheme == "light" ? styles.safeArea_light : styles.safeArea_dark
      }>
      <MyCalendarProvider>{children}</MyCalendarProvider>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeArea_dark: {
    flex: 1,
    backgroundColor: Colors.primary.s600,
    alignItems: "center",
  },
});
