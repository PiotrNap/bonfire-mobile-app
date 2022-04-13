import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import {
  appContext,
  eventCreationContext,
  myCalendarContext,
} from "contexts/contextApi"
import { Typography, Colors, Sizing, Outlines } from "../../styles"
import { getRecurringMonthDays } from "lib/helpers"
import { getDate, getTime, isPastDate } from "lib/utils"
import { monthsByName } from "common/types/calendarTypes"

const weekDays: string[] = ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"]

type SelectedWeekDays = { [key: string]: any }
const _selectedWeekDays: SelectedWeekDays = {
  date: null,
  0: false,
  1: false,
  2: false,
  3: false,
  4: false,
  5: false,
  6: false,
}

export const WeekDayNames = ({
  isNewEventCalendar = false,
}: {
  isNewEventCalendar?: boolean
  customCallback?: (arg: boolean) => void
}) => {
  const { calendarHeader } = myCalendarContext()
  const { setSelectedDays, selectedDays, setSelectedWeek, selectedWeekDays } =
    eventCreationContext()
  const { colorScheme } = appContext()
  const { month, year } = calendarHeader
  const isDarkMode = colorScheme === "dark"
  const isSelectedDay = (index: number) =>
    !!selectedWeekDays.length &&
    !!selectedWeekDays[currMonthSelectedWeekIndex()]?.[index]
  const currentMonthSelectedWeek = React.useCallback(() => {
    var week = selectedWeekDays.find(
      (week) => week.date === getTime(year, monthsByName[month])
    )

    if (week) return week
    return null
  }, [month, year])
  const currMonthSelectedWeekIndex = React.useCallback(() => {
    return selectedWeekDays.findIndex(
      (week) => week.date === getTime(year, monthsByName[month])
    )
  }, [month, year])
  const isUnavailableWeek = (i: number) => !recurringAvailableDays(i).length

  const dayButtonStyle = [
    {
      backgroundColor: isDarkMode ? Colors.primary.neutral : "white",
      borderColor: Colors.applyOpacity(
        isDarkMode ? Colors.neutral.s800 : Colors.primary.s350,
        0.4
      ),
    },
  ]
  const recurringAvailableDays = (i: number) => {
    var arrOfDays: number[] = getRecurringMonthDays(i, year, month)
    return arrOfDays.filter((date) => !isPastDate(year, month, getDate(date)))
  }
  // we don't want to select days in the past
  const onPress = (index: number): void => {
    let selectRecurring: boolean = recurringAvailableDays(index)
      .map((num: number) => !!selectedDays?.[num])
      .includes(false)

    setSelectedDays(recurringAvailableDays(index), selectRecurring)

    let newSelectedWeek = currentMonthSelectedWeek()

    if (newSelectedWeek) {
      newSelectedWeek[index] = selectRecurring ? true : !newSelectedWeek[index]

      setSelectedWeek(newSelectedWeek)
    } else {
      newSelectedWeek = Object.assign({}, _selectedWeekDays)
      newSelectedWeek.date = getTime(year, monthsByName[month])
      newSelectedWeek[index] = selectRecurring ? true : !selectedWeekDays[index]

      setSelectedWeek(newSelectedWeek)
    }
  }

  return (
    <View style={styles.container}>
      <View style={styles.weekDays}>
        {weekDays.map((day, i) =>
          !isNewEventCalendar || isUnavailableWeek(i) ? (
            <View key={`day-${i}`} style={[styles.dayContainer]}>
              <View style={styles.dayPlaceholder}>
                <Text
                  style={[
                    styles.dayTitle,
                    {
                      color: Colors.primary.s600,
                    },
                  ]}>
                  {day}
                </Text>
              </View>
            </View>
          ) : (
            <Pressable
              key={i}
              style={[styles.dayContainer]}
              hitSlop={5}
              onPress={() => onPress(i)}>
              <View
                style={[
                  styles.dayButton,
                  dayButtonStyle,
                  isSelectedDay(i) && {
                    ...styles.selectedDayButton,
                    borderColor: Colors.primary.s600,
                  },
                ]}>
                <Text
                  style={[
                    styles.dayTitle,
                    {
                      color: isSelectedDay(i)
                        ? Colors.primary.neutral
                        : Colors.primary.s600,
                    },
                  ]}>
                  {day}
                </Text>
              </View>
            </Pressable>
          )
        )}
      </View>
    </View>
  )
}

const styles = StyleSheet.create({
  container: {},
  weekDays: {
    flexDirection: "row",
    justifyContent: "space-evenly",
  },
  dayTitle: {
    ...Typography.body.x30,
    ...Typography.roboto.medium,
    zIndex: 2,
    // looks like the font is slightly moved to left
    textAlign: "center",
    marginLeft: Sizing.x1,
  },
  dayContainer: {
    width: `${100 / 7}%`,
    height: `${100 / 6}%`,
    justifyContent: "flex-start",
    alignItems: "center",
  },
  dayPlaceholder: {
    width: 33,
    height: 33,
    alignItems: "center",
    justifyContent: "center",
  },
  dayButton: {
    width: 33,
    height: 33,
    alignItems: "center",
    justifyContent: "center",
    borderRadius: 999,
    borderWidth: Outlines.borderWidth.thin,
    ...Outlines.shadow.base,
  },
  selectedDayButton: {
    backgroundColor: Colors.primary.s600,
  },
})
