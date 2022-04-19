import * as React from "react"
import { Pressable, Text, View, StyleSheet } from "react-native"
import { Colors, Outlines, Typography } from "styles/index"
import { Day } from "interfaces/myCalendarInterface"
import { getTime, isPastDate } from "lib/utils"
import { monthsByName } from "common/types/calendarTypes"
import { appContext } from "contexts/contextApi"

export interface AvailabilityDayProps extends Day {
  number: number
  month: string
  year: number
  isSelectedDay: boolean
  onPressCallback: (val: number) => void
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
  const { colorScheme } = appContext()
  const isDarkMode = colorScheme === "dark"

  const dayButtonStyle = [
    styles.dayButton,
    {
      backgroundColor: isDarkMode ? Colors.primary.neutral : "white",
    },
    {
      borderColor: isSelectedDay
        ? Colors.primary.s600
        : Colors.applyOpacity(
            isDarkMode ? Colors.neutral.s800 : Colors.primary.s350,
            0.4
          ),
    },
    isSelectedDay && styles.selectedDayButton,
  ]

  // disable past days and the current one
  return isPastDate(year, month, number) ? (
    <View style={styles.dayContainer}>
      <View
        style={[
          styles.dayButton,
          {
            backgroundColor: Colors.neutral.s300,
            borderColor: Colors.neutral.s300,
          },
        ]}>
        <Text
          style={[
            styles.dayNumber,
            {
              color: Colors.neutral.s100,
            },
          ]}>
          {number}
        </Text>
      </View>
    </View>
  ) : (
    <Pressable
      style={[styles.dayContainer]}
      hitSlop={5}
      onPress={() =>
        onPressCallback(getTime(year, monthsByName[month], number))
      }>
      <View style={dayButtonStyle}>
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
  )
}

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
    ...Outlines.shadow.base,
  },
  selectedDayButton: {
    backgroundColor: Colors.primary.s600,
  },
})

export const AvailabilityDay = React.memo(_AvailabilityDay)
