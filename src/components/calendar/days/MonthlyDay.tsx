import * as React from "react"
import { Pressable, Text, View, StyleSheet } from "react-native"

import { Buttons, Colors, Sizing, Typography } from "styles/index"
import { DotIcon } from "icons/index"
import { Day } from "interfaces/myCalendarInterface"
import { applyOpacity } from "../../../styles/colors"
import { getTime } from "lib/utils"
import { monthsByName } from "common/types/calendarTypes"

export interface MonthlyDayProps extends Day {
  activeDay: number | null
  year: number
  month: string
  updateActiveDay: (arg: number | null) => void
  setSelectedDay?: (arg: any) => any
  isPastDate?: boolean
  showPastDate?: boolean
}

/**
 *  @description
 *    This component is being used to display days on a regular 'main' calendar.
 */

export const MonthlyDay = ({
  number,
  year,
  month,
  activeDay,
  updateActiveDay,
  events,
  showPastDate,
  isPastDate,
}: MonthlyDayProps) => {
  const hasActiveEvents = !!events?.find(
    (e) => e.type === "active slot"
  )
  const hasScheduledSlots = !!events?.find(
    (e) => e.type !== "active slot"
  )

  const dayInTime =
    year &&
    month &&
    number &&
    getTime(year, monthsByName[month], number)
  const isActiveDay = activeDay === dayInTime

  const onPress = () => {
    updateActiveDay(isActiveDay ? null : number)
  }

  const TextComponent = () => (
    <Text style={styles.dayButtonText}>{number}</Text>
  )

  return React.useMemo(
    () => (
      <Pressable
        onPress={onPress}
        hitSlop={Sizing.x5}
        style={styles.dayButton}>
        {isActiveDay ? (
          <View style={styles.selectionIndicator}>
            <TextComponent />
          </View>
        ) : (
          <TextComponent />
        )}
        {isPastDate && !showPastDate ? null : (
          <>
            {hasActiveEvents && hasScheduledSlots && (
              <>
                <DotIcon
                  style={[styles.icon, styles.leftIcon]}
                  fill="#60A5FA"
                  stroke="none"
                />
                <DotIcon
                  style={[styles.icon, styles.rightIcon]}
                  fill="#FCD34D"
                  stroke="none"
                />
              </>
            )}
            {hasActiveEvents && !hasScheduledSlots && (
              <DotIcon
                style={styles.icon}
                fill="#60A5FA"
                stroke="none"
              />
            )}
            {hasScheduledSlots && !hasActiveEvents && (
              <DotIcon
                style={styles.icon}
                fill="#FCD34D"
                stroke="none"
              />
            )}
          </>
        )}
      </Pressable>
    ),
    [isActiveDay]
  )
}

const styles = StyleSheet.create({
  dotsWrapper: {
    zIndex: 5,
    flexDirection: "row",
    marginTop: 2,
    width: "50%",
    justifyContent: "space-evenly",
  },
  selectionIndicator: {
    width: Sizing.x42,
    height: Sizing.x42,
    alignItems: "center",
    justifyContent: "center",
    borderRadius: 999,
    backgroundColor: applyOpacity(Colors.primary.s350, 0.34),
  },
  dayButtonText: {
    ...Typography.body.x30,
    ...Typography.roboto.medium,
    textAlign: "center",
    zIndex: 3,
    color: Colors.primary.s600,
  },
  dayButton: {
    width: `${100 / 7}%`,
    height: `${100 / 6}%`,
    justifyContent: "center",
    alignItems: "center",
  },
  icon: {
    ...Buttons.circular.primary,
    backgroundColor: "transparent",
    height: Sizing.x7,
    width: Sizing.x7,
    position: "absolute",
    bottom: "10%",
  },
  leftIcon: {
    left: "35%",
  },
  rightIcon: {
    right: "35%",
  },
})
