import * as React from "react";
import { Pressable, Text, View, StyleSheet } from "react-native";
import { Colors, Outlines, Sizing, Typography } from "styles/index";
import { Day } from "interfaces/myCalendarInterface";
import { getTime } from "lib/utils";
import { monthsByName } from "common/types/calendarTypes";

export interface AvailabilityDayProps extends Day {
  number: number;
  month: string;
  year: number;
  isSelectedDay: boolean;
  onPressCallback: (val: number) => void;
}

/**
 *  This day components is being used in calendar for selecting
 *  availabilities when creating new event as organizer.
 */
export const _AvailabilityDay = ({
  number,
  month,
  year,
  onPressCallback,
  isSelectedDay,
}: AvailabilityDayProps) => {
  return (
    <Pressable
      style={[styles.dayContainer]}
      hitSlop={5}
      onPress={() =>
        onPressCallback(getTime(year, monthsByName[month], number))
      }>
      <View
        style={[styles.dayButton, isSelectedDay && styles.selectedDayButton]}>
        <Text
          style={[
            styles.dayNumber,
            {
              color: isSelectedDay
                ? Colors.primary.neutral
                : Colors.primary.s600,
            },
          ]}>
          {number}
        </Text>
      </View>
    </Pressable>
  );
};

const styles = StyleSheet.create({
  dayNumber: {
    ...Typography.body.x30,
    ...Typography.roboto.medium,
    zIndex: 2,
    textAlign: "center",
  },
  dayContainer: {
    width: `${100 / 7}%`,
    height: `${100 / 6}%`,
    justifyContent: "flex-start",
    alignItems: "center",
  },
  dayButton: {
    borderRadius: 999,
    width: 33,
    height: 33,
    alignItems: "center",
    justifyContent: "center",
    borderWidth: Outlines.borderWidth.thin,
    borderColor: Colors.applyOpacity(Colors.neutral.s400, 0.4),
    backgroundColor: "white",
    ...Outlines.shadow.base,
  },
  selectedDayButton: {
    backgroundColor: Colors.primary.s600,
  },
});

export const AvailabilityDay = React.memo(_AvailabilityDay);
