import * as React from "react"
import { View, Text, StyleSheet, Pressable, Animated } from "react-native"

import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { CalendarSectionTitles, Event } from "interfaces/myCalendarInterface"
import { getDate, getDigitalTime, getLocaleTimezone, getMonth } from "lib/utils"
import { months } from "common/types/calendarTypes"
import { RightArrowIcon } from "icons/index"

export interface CalendarEventsDetailProps extends Event {
  setHighlightedDay: React.Dispatch<any>
  highlightedDay: any
  listLength: number
  index: number
  availableAt: number
  listSection: CalendarSectionTitles
  bookedDate: Date | undefined
  bookedDuration: number | undefined
  eventTitle: string | undefined
  eventDescription: string | undefined
  maxAvailableMonthDate: number | undefined
  minAvailableMonthDate: number | undefined
  currentSelectedDay: Date | undefined
}

export const CalendarEventsDetail = ({
  type,
  index,
  fromDate,
  // toDate,
  availableAt,
  fromTimeSlot,
  toTimeSlot,
  listLength,
  setHighlightedDay,
  highlightedDay,
  listSection,
  bookedDate,
  bookedDuration,
  organizerAlias,
  attendeeAlias,
  eventTitle,
  eventDescription,
  availabilityDate,
  maxAvailableMonthDate,
  minAvailableMonthDate,
  currentSelectedDay,
}: CalendarEventsDetailProps) => {
  const animatedMargin = React.useRef(new Animated.Value(-65)).current
  const animatedValue = parseInt(JSON.stringify(animatedMargin))

  const bookedTime =
    bookedDate && bookedDuration
      ? new Date(bookedDate).getTime() + bookedDuration
      : null
  const fromTimeDigit = getDigitalTime(bookedDate || fromTimeSlot)
  const toTimeDigit = getDigitalTime(bookedTime || toTimeSlot)
  const eventDay =
    type === "active slot"
      ? getDate(availableAt)
      : getDate(bookedDate || availabilityDate || fromDate || "")
  const eventMonth = months[getMonth(bookedDate || fromTimeSlot)]
  const fromDayActiveEvent = getDate(minAvailableMonthDate)
  const toDayActiveEvent = getDate(maxAvailableMonthDate)

  const fromDateToDateEvent =
    type === "active slot" &&
    (listSection === CalendarSectionTitles.thisMonth ||
      (!currentSelectedDay && !listSection))

  const onDateCardPress = () => {
    // when we click on the last card, return
    if (index === listLength - 1) return

    if (
      highlightedDay.listSection === listSection &&
      highlightedDay.index - 1 === index &&
      animatedValue === Sizing.x65
    ) {
      console.log("here") // when someone clicks on the same card, just pull it back to top
      setHighlightedDay({ listSection: "", index: null })
    } else {
      console.log("or here") // on press, set the index of card bellow the one that was clicked,
      // because that's the one that needs to move down
      setHighlightedDay({ listSection, index: index + 1 })
    }
  }
  const onArrowPress = () => console.log("pressed the arrow!")

  const animateToTop = () => {
    Animated.timing(animatedMargin, {
      toValue: -Sizing.x65,
      duration: 200,
      useNativeDriver: false,
    }).start()
  }
  const animateToBottom = () => {
    Animated.timing(animatedMargin, {
      toValue: 0,
      duration: 200,
      useNativeDriver: false,
    }).start()
  }

  React.useEffect(() => {
    if (
      listSection === highlightedDay.listSection &&
      index === highlightedDay.index
    ) {
      animateToBottom()
    } else if (Number(animatedMargin) !== 0) {
      animateToTop()
    }
  }, [highlightedDay])

  const cardStyle = [
    styles.container,
    {
      backgroundColor:
        type === "active slot"
          ? Colors.blueCalendarCard.s300
          : Colors.yellowCalendarCard.s300,
    },
    {
      zIndex: index,
      marginTop: index === 0 ? 0 : animatedMargin,
    },
  ]
  const textColor = {
    color:
      type === "active slot"
        ? Colors.blueCalendarCard.s400
        : Colors.yellowCalendarCard.s400,
  }
  const darkerTextColor = {
    color:
      type === "active slot"
        ? Colors.blueCalendarCard.s500
        : Colors.yellowCalendarCard.s500,
  }

  return (
    <Pressable onPress={onDateCardPress}>
      <Animated.View style={cardStyle}>
        <View style={styles.upperContainer}>
          <View style={styles.dateHolder}>
            <Text style={[styles.dateDay, textColor]}>
              {fromDateToDateEvent
                ? (fromDayActiveEvent < 10
                    ? "0" + fromDayActiveEvent
                    : fromDayActiveEvent) +
                  "-" +
                  (toDayActiveEvent < 10
                    ? " " + toDayActiveEvent
                    : toDayActiveEvent)
                : eventDay < 10
                ? "0" + eventDay
                : eventDay}
            </Text>
            <Text style={[styles.dateMonth, darkerTextColor]}>
              {eventMonth}
            </Text>
          </View>
          <View style={styles.hourHolder}>
            <Text style={[styles.hours, textColor]}>
              {fromTimeDigit} - {toTimeDigit} {getLocaleTimezone()}
            </Text>
          </View>
        </View>
        <View style={styles.bottomContainer}>
          <View style={styles.eventDetail}>
            <Text style={[styles.eventDetailText, textColor]}>
              {eventTitle}
            </Text>
            {type === "scheduled slot" ? (
              <Text style={[styles.eventDetailText, textColor]}>
                Attendee: {attendeeAlias}
              </Text>
            ) : type === "booked slot" ? (
              <Text style={[styles.eventDetailText, textColor]}>
                Organizer: {organizerAlias}
              </Text>
            ) : (
              <Text
                ellipsizeMode="tail"
                numberOfLines={1}
                style={[styles.eventDetailText, textColor]}>
                {eventDescription}
              </Text>
            )}
          </View>
          <Pressable
            style={Buttons.applyOpacity(styles.iconWrapper)}
            onPress={onArrowPress}>
            <RightArrowIcon
              stroke={darkerTextColor.color}
              strokeWidth="2.5"
              width="24"
              height="24"
            />
          </Pressable>
        </View>
      </Animated.View>
    </Pressable>
  )
}

const styles = StyleSheet.create({
  container: {
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.lifted,
    paddingVertical: Sizing.x10,
    paddingHorizontal: Sizing.x12,
    marginHorizontal: Sizing.x8,
    alignSelf: "center",
    width: "95%",
  },
  upperContainer: {
    flexDirection: "row",
    justifyContent: "space-between",
    alignItems: "baseline",
  },
  bottomContainer: {
    flexDirection: "row",
  },
  dateHolder: {
    flexDirection: "row",
    alignItems: "baseline",
  },
  hourHolder: {
    justifyContent: "flex-end",
    alignItems: "baseline",
  },
  hours: {
    ...Typography.subHeader.x20,
    ...Typography.fontWeight.semibold,
  },
  dateDay: {
    ...Typography.header.x70,
    letterSpacing: -2,
    marginRight: Sizing.x3,
  },
  dateMonth: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Medium",
    lineHeight: Sizing.x45,
  },
  eventDetail: {
    flex: 1,
    marginLeft: Sizing.x5,
  },
  eventDetailText: {
    ...Typography.body.x20,
    ...Typography.fontWeight.semibold,
    lineHeight: Sizing.x30,
    maxWidth: "90%",
  },
  iconWrapper: {
    marginRight: Sizing.x1,
    flexDirection: "row",
    alignItems: "center",
  },
})
