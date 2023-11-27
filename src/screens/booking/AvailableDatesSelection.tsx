import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { BookingStackParamList, DEEP_LINKING_URLS } from "common/types/navigationTypes"
import { Colors, Outlines, Sizing } from "styles/index"
import { appContext, bookingContext } from "contexts/contextApi"
import { MonthlyWrapper } from "components/calendar"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"
import { Checkbox } from "components/forms/Checkbox"
import { useGoogleAuth } from "lib/hooks/useGoogleAuth"
import { createNestedPath } from "lib/navigation"
import { Calendar } from "react-native-calendars"
import { DateData, MarkedDates } from "react-native-calendars/src/types"
import { isPastDate } from "lib/utils"
import { applyOpacity } from "../../styles/colors"
import {
  convertFromEventAvailability,
  generateTimeSlotsForDateInMilliseconds,
} from "lib/helpers"

type Props = StackScreenProps<BookingStackParamList, "Available Event Dates Selection">

const MARKED_DATE = {
  customStyles: {
    container: {
      backgroundColor: applyOpacity(Colors.blueCalendarCard.s400, 0.5), // New background color for selected date
    },
    text: {
      color: Colors.primary.neutral, // Color of the text for selected state
    },
  },
  selected: false,
}

export const AvailableDatesSelection = ({ navigation, route }: Props) => {
  const { title, image, eventCardColor, eventTitleColor, availabilities } =
    route.params?.event
  const {
    setPickedDateSlots,
    setPickedDateSlotsMinDuration,
    setPickedDate,
    pickedDate,
    resetBookingState,
    setCreateGoogleCalEvent,
  } = bookingContext()
  const { colorScheme, validGoogleOAuth } = appContext()
  // const [acceptedCheckbox, setAcceptedChecbox] = React.useState<boolean>(false)
  const [selectedDate, setSelectedDate] = React.useState<string>(pickedDate)
  const [markedDates, setMarkedDates] = React.useState<MarkedDates>({})
  const [error, setError] = React.useState<any>({ isVisible: false, type: "" })
  const [timeSlots, setTimeSlots] = React.useState<any>(null)
  const isLightMode = colorScheme === "light"
  const lightOrDarkColor = isLightMode ? Colors.primary.s800 : Colors.primary.neutral

  React.useEffect(() => {
    const { dates, timeWindows } = convertFromEventAvailability(availabilities, true)
    // let timeSlots = generateTimeSlotsForDateInMilliseconds([dates, timeWindows], date)
    let newDates = dates
    if (pickedDate) {
      newDates[pickedDate] = {
        //@ts-ignore
        utcDate: newDates[pickedDate].utcDate,
        selected: true,
        customStyles: {
          ...newDates[pickedDate].customStyles,
          container: {
            backgroundColor: applyOpacity(Colors.blueCalendarCard.s400, 0.5), // New background color for selected date
          },
        },
      }
    }
    setMarkedDates(dates)
    setTimeSlots(timeWindows)
  }, [availabilities])

  // @TODO after beta release
  // const googleCallback = () => {
  //   setCreateGoogleCalEvent(acceptedCheckbox)
  //   navigation.navigate("Available Times", route.params)
  // }
  // const { isRequesting, isInitialRequesting, startGoogleAuthentication } = useGoogleAuth(
  //   googleCallback,
  //   setError
  // )

  const onNextPress = async () => {
    if (error.isVisible) setError({ isVisible: false, type: "" })

    //@ts-ignore
    setPickedDate(markedDates[selectedDate].utcDate)
    let timeSlotMinDurations = []
    const slotsForGivenDate = timeSlots
      .filter((timeSlot) => {
        if (timeSlot.fromDate === selectedDate) {
          timeSlotMinDurations.push(timeSlot.minDuration)
          return true
        }
        return false
      })
      .map((timeSlot) => timeSlot.slots)
    setPickedDateSlots(slotsForGivenDate)
    setPickedDateSlotsMinDuration(timeSlotMinDurations)
    // if (acceptedCheckbox && !validGoogleOAuth) {
    //   try {
    //     await startGoogleAuthentication(
    //       createNestedPath([
    //         DEEP_LINKING_URLS.NAVIGATION,
    //         DEEP_LINKING_URLS.BROWSE,
    //         DEEP_LINKING_URLS.AVAILABLE_TIMES,
    //       ])
    //     )
    //   } catch (e) {
    //     console.error("New error ", e)
    //     setError({ isVisible: true, type: "GoogleOauth" })
    //   }
    // } else {
    // acceptedCheckbox && setCreateGoogleCalEvent(acceptedCheckbox)
    // }
    navigation.navigate("Available Times", route.params)
  }
  const onBackNavigationPress = () => {
    resetBookingState()
    navigation.navigate("Search List")
  }
  // const onCheckBoxPress = () => {
  //   setError({ isVisible: false, type: "" })
  //   // setAcceptedChecbox((prev) => !prev)
  // }

  const onDayPress = (date: DateData) => {
    if (isPastDate(date.year, date.month - 1, date.day)) return
    if (!markedDates[date.dateString]) return // no available slot for this date

    let newMarkedDates = markedDates
    if (!selectedDate) {
      newMarkedDates[date.dateString] = {
        //@ts-ignore
        utcDate: newMarkedDates[date.dateString].utcDate,
        selected: true,
        customStyles: {
          ...newMarkedDates[date.dateString].customStyles,
          container: {
            backgroundColor: Colors.primary.s600,
          },
        },
      }
      setSelectedDate(date.dateString)
    } else if (selectedDate === date.dateString) {
      newMarkedDates[date.dateString] = {
        //@ts-ignore
        utcDate: newMarkedDates[date.dateString].utcDate,
        selected: false,
        customStyles: {
          ...newMarkedDates[date.dateString].customStyles,
          container: {
            backgroundColor: applyOpacity(Colors.blueCalendarCard.s400, 0.5),
          },
        },
      }
      setSelectedDate("")
    } else {
      newMarkedDates[selectedDate] = {
        //@ts-ignore
        utcDate: newMarkedDates[date.dateString].utcDate,
        selected: false,
        customStyles: {
          ...newMarkedDates[date.dateString].customStyles,
          container: {
            backgroundColor: applyOpacity(Colors.blueCalendarCard.s400, 0.5),
          },
        },
      }
      newMarkedDates[date.dateString] = {
        //@ts-ignore
        utcDate: newMarkedDates[date.dateString].utcDate,
        selected: true,
        customStyles: {
          ...newMarkedDates[date.dateString].customStyles,
          container: {
            backgroundColor: Colors.primary.s600,
          },
        },
      }
      setSelectedDate(date.dateString)
    }

    setMarkedDates(newMarkedDates)
  }

  return (
    <EventBookingLayout
      onBackPress={onBackNavigationPress}
      screenHeader={"Pick a Date"}
      screenSubHeader={"Select one of the available dates highlighted in blue"}
      eventCardColor={eventCardColor}
      eventCardImage={image}
      eventCardTitle={title}
      eventCardTitleColor={eventTitleColor}>
      <View style={styles.calendarWrapper}>
        <Calendar
          theme={{
            calendarBackground: !isLightMode
              ? Colors.primary.neutral
              : Colors.neutral.s600,
            textSectionTitleColor: "#b6c1cd",
            textSectionTitleDisabledColor: "#d9e1e8",
            selectedDayBackgroundColor: Colors.primary.s800,
            selectedDayTextColor: lightOrDarkColor,
            todayTextColor: "#00adf5",
            dayTextColor: "#2d4150",
            textDisabledColor: "#d9e1e8",
            dotColor: "#00adf5",
            selectedDotColor: "#ffffff",
            arrowColor: lightOrDarkColor,
            disabledArrowColor: "#d9e1e8",
            monthTextColor: "blue",
            indicatorColor: "blue",
            textDayFontFamily: "Roboto-Regular",
            textMonthFontFamily: "Roboto-Bold",
            textDayHeaderFontFamily: "Roboto-Medium",
            textDayFontSize: Sizing.x20,
            textMonthFontSize: 16,
            textDayHeaderFontSize: 16,
          }}
          onDayPress={onDayPress}
          markedDates={markedDates}
          markingType={"custom"}
          style={styles.calendar}
          // enableSwipeMonths
          // Other props and customization...
        />
      </View>
      {/*
      <View style={styles.messageWrapper}>
        <Checkbox onCheckBoxPress={onCheckBoxPress} acceptedCheckbox={acceptedCheckbox}>
          Schedule this event on my Google calendar.
        </Checkbox>
      </View>
      */}
      <View style={styles.buttonContainer}>
        <FullWidthButton
          onPressCallback={onNextPress}
          text={"Next"}
          colorScheme={colorScheme}
          disabled={!selectedDate}
          loadingIndicator={false}
        />
      </View>
    </EventBookingLayout>
  )
}

const styles = StyleSheet.create({
  calendarWrapper: {
    width: "90%",
  },
  calendar: {
    width: "100%",
    marginTop: Sizing.x5,
    paddingVertical: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.lifted,
  },
  buttonContainer: {
    alignItems: "center",
    justifyContent: "center",
    width: "90%",
    marginBottom: Sizing.x10,
    marginTop: Sizing.x5,
  },
  messageWrapper: {
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    marginTop: Sizing.x5,
    alignItems: "center",
  },
})
