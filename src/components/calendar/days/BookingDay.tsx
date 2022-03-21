import * as React from "react";
import { Pressable, Text, View, StyleSheet } from "react-native";

import { Colors, Outlines, Sizing, Typography } from "styles/index";
import { PartiallyBookedDay } from "icons/index";
import { Day } from "interfaces/myCalendarInterface";
import { getTime } from "lib/utils";
import { monthsByName } from "common/types/calendarTypes";
import { bookingContext } from "contexts/contextApi";

export interface BookingDayProps extends Day {
  year?: number;
  month: string;
  activeDay: number | null;
  isAvailable?: boolean;
  setActiveDay: React.Dispatch<React.SetStateAction<number | null>>;
  setSelectedDay?: (arg: any) => any;
}

/**
 * @description
 *  This day components is being used in calendar for selecting
 *  available days while booking an event.
 */

export const _BookingDay = ({
  number,
  year,
  month,
  activeDay,
  scheduledEvents,
  availabilities,
  isAvailable,
}: BookingDayProps) => {
  const { pickedDate, setPickedDate } = bookingContext();

  const dayInTime = getTime(year, monthsByName[month], number);
  const isActiveDay =
    (activeDay && activeDay === number) || pickedDate === dayInTime;

  // Whenever the first scheduled event starts at first available time,
  // and the last scheduled event ends at the last available time
  const isFullyBooked = React.useCallback(
    () =>
      scheduledEvents != null &&
      availabilities != null &&
      scheduledEvents[0].fromTime === availabilities[0].fromTime &&
      scheduledEvents[scheduledEvents.length - 1].toTime ===
        availabilities[availabilities.length - 1].toTime,
    [availabilities, scheduledEvents]
  );

  const isPartiallyBooked = !isFullyBooked && scheduledEvents != null;
  const isFullyAvailable = React.useCallback(
    () =>
      isAvailable ||
      (availabilities != null &&
        availabilities.length > 0 &&
        scheduledEvents == null) ||
      (scheduledEvents != null && scheduledEvents.length === 0),
    [availabilities, scheduledEvents]
  );
  const isNonAvailableDay =
    !isFullyBooked && !isFullyAvailable && !isPartiallyBooked;

  const onPress = () => {
    // Do not select it
    if (!isAvailable) return;

    // When already selected, deselect it
    if (pickedDate === dayInTime) {
      setPickedDate(null);
    } else if (
      pickedDate !== dayInTime ||
      (activeDay !== number && !isNonAvailableDay && !isFullyBooked)
    ) {
      setPickedDate(dayInTime);
    }
  };

  return (
    <Pressable style={[styles.dayContainer]} hitSlop={5} onPress={onPress}>
      <View
        style={[
          styles.dayButton,
          {
            backgroundColor: isActiveDay
              ? Colors.primary.s600
              : isFullyBooked()
              ? Colors.booked
              : isFullyAvailable()
              ? Colors.available
              : "transparent",
          },
          !isFullyBooked && availabilities && { ...Outlines.shadow.base },
        ]}>
        <Text
          style={[
            styles.dayNumber,
            {
              color: isActiveDay ? Colors.primary.neutral : Colors.primary.s600,
            },
          ]}>
          {number}
        </Text>
        {isPartiallyBooked && !isActiveDay && (
          <PartiallyBookedDay
            width={34}
            height={34}
            style={styles.partiallyBookedDay}
          />
        )}
      </View>
    </Pressable>
  );
};

const styles = StyleSheet.create({
  dayNumber: {
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
  dayButton: {
    borderRadius: 999,
    width: 33,
    height: 33,
    alignItems: "center",
    justifyContent: "center",
  },
  partiallyBookedDay: {
    position: "absolute",
    bottom: 0,
    borderRadius: 999,
    height: 33,
    width: 33,
  },
});

export const BookingDay = React.memo(_BookingDay);
