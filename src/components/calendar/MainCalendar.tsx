import * as React from "react";
import { WeekDayNames } from "./WeekDayNames";
import { Pressable, StyleSheet, Text, View } from "react-native";
import { Sizing, Colors, Outlines, Typography } from "styles/index";
import { MonthlyWrapper } from "./MonthlyWrapper";
import { myCalendarContext } from "contexts/contextApi";

export const MainCalendar = () => {
  const { calendar, changeMonthHeader, calendarHeader } = myCalendarContext();
  const onPreviousPress = () => {
    const calendarHeader = {
      month: calendar[0].name,
      year: calendar[0].year,
    };
    changeMonthHeader(calendarHeader);
  };
  const onNextPress = () => {
    const calendarHeader = {
      month: calendar[2].name,
      year: calendar[2].year,
    };
    changeMonthHeader(calendarHeader);
  };
  return (
    <View style={styles.container}>
      <View style={styles.headerContainer}>
        <View style={styles.header}>
          <Text style={styles.headerMonth_light}>{calendarHeader.month}</Text>
          <Text style={styles.headerYear_light}>{calendarHeader.year}</Text>
        </View>
        <View style={styles.header}>
          <Pressable
            style={{
              padding: 12,
              margin: 5,
              backgroundColor: Colors.primary.s400,
            }}
            onPress={onPreviousPress}>
            <Text>Prev</Text>
          </Pressable>
          <Pressable
            style={{
              padding: 12,
              margin: 5,
              backgroundColor: Colors.primary.s400,
            }}
            onPress={onNextPress}>
            <Text>N</Text>
          </Pressable>
        </View>
      </View>
      <View style={styles.calendar}>
        <WeekDayNames />
        <MonthlyWrapper />
      </View>
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 6,
    marginHorizontal: Sizing.x10,
  },
  calendar: {
    height: "100%",
    width: "100%",
    alignItems: "center",
    backgroundColor: "white",
    borderRadius: Outlines.borderRadius.small,
    borderWidth: Outlines.borderWidth.base,
    borderColor: Colors.neutral.s400,
  },
  headerContainer: {
    width: "90%",
    flexDirection: "row",
  },
  header: {
    width: "50%",
    flexDirection: "row",
    marginHorizontal: Sizing.x15,
    marginVertical: Sizing.x5,
    alignSelf: "flex-end",
  },
  headerMonth_light: {
    ...Typography.header.x40,
    color: Colors.primary.s600,
    paddingRight: 5,
  },
  headerMonth_dark: {
    ...Typography.header.x40,
    color: Colors.primary.neutral,
    paddingRight: 5,
  },
  headerYear_light: {
    ...Typography.header.x40,
    color: Colors.primary.s300,
    paddingRight: 5,
  },
  headerYear_dark: {
    ...Typography.header.x40,
    color: Colors.primary.neutral,
    paddingRight: 5,
  },
});
