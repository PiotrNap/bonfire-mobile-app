import { useFocusEffect } from "@react-navigation/native"
import { Users } from "Api/Users"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import {
  createTimelineEvents,
  getCalendarDate,
  getMarkedDatesFromUserCalendarData,
  showErrorToast,
} from "lib/helpers"
import * as React from "react"
import { ActivityIndicator, StyleSheet, View } from "react-native"
import {
  CalendarProvider,
  ExpandableCalendar,
  TimelineEventProps,
  TimelineList,
} from "react-native-calendars"
import { DateData, MarkedDates } from "react-native-calendars/src/types"
import { SafeAreaView } from "react-native-safe-area-context"
import { Colors, Sizing } from "styles/index"

const INITIAL_TIME = { hour: new Date().getHours(), minutes: new Date().getMinutes() }

export const MyCalendarScreen = ({ navigation }: any) => {
  const { colorScheme, deviceTopInsent } = appContext()
  const isLightMode = colorScheme === "light"
  const lightOrDarkColor = isLightMode ? Colors.primary.s800 : Colors.primary.neutral

  const { id } = React.useContext(ProfileContext)
  const [marked, setMarked] = React.useState<MarkedDates>({})
  const [timeLineEvents, setTimeLineEvents] = React.useState<TimelineEventProps[]>([])
  const [currentMonth, setCurrentMonth] = React.useState<number>(new Date().getMonth())
  const [currentYear, setCurrentYear] = React.useState<number>(new Date().getFullYear())
  const [isLoading, setIsLoading] = React.useState<boolean>(false)

  const fetchCalendarData = async (year, month) => {
    setIsLoading(true)
    try {
      let calendarData = await Users.getUserCalendarEvents(
        id,
        new Date(new Date().setFullYear(year, month, 15)) // to be safe we choose 15th
      )
      if (!calendarData) throw new Error("Unable to get your calander data")

      if (
        !calendarData.events?.length &&
        !calendarData.bookedSlots?.length &&
        !calendarData.scheduledSlots?.length
      )
        return

      const markedDates = getMarkedDatesFromUserCalendarData(calendarData)
      const { timelineEvents } = createTimelineEvents(calendarData)

      setTimeLineEvents(timelineEvents)
      setMarked(markedDates)
      setCurrentMonth(month - 1)
      setCurrentYear(year)
    } catch (e) {
      showErrorToast({error: e, topOffset: deviceTopInsent})
    } finally {
      setIsLoading(false)
    }
  }

  useFocusEffect(
    React.useCallback(() => {
      fetchCalendarData(new Date().getFullYear(), new Date().getMonth())
    }, [id])
  )

  const currentDate = getCalendarDate()
  const onDateChanged = (date) => console.log(date)
  const onMonthChange = async (date: DateData) => {
    fetchCalendarData(date.year, date.month - 1)
  }
  return (
    <SafeAreaView style={styles.calendarContainer}>
      <CalendarProvider
        date={currentDate}
        onDateChanged={onDateChanged}
        onMonthChange={onMonthChange}
        showTodayButton
        disabledOpacity={0.6}
        // numberOfDays={3}
      >
        <ExpandableCalendar
          key={colorScheme}
          displayLoadingIndicator={isLoading}
          indicatorStyle={isLightMode ? "default" : "white"}
          firstDay={1}
          markedDates={marked}
          markingType={"multi-dot"}
          theme={{
            calendarBackground: !isLightMode
              ? Colors.neutral.s600
              : Colors.primary.neutral,
            textSectionTitleColor: lightOrDarkColor,
            textSectionTitleDisabledColor: "#d9e1e8",
            selectedDayBackgroundColor: Colors.primary.s600,
            selectedDayTextColor: Colors.primary.neutral,
            todayTextColor: "#00adf5",
            dayTextColor: lightOrDarkColor,
            textDisabledColor: "#d9e1e8",
            arrowColor: lightOrDarkColor,
            disabledArrowColor: "#d9e1e8",
            monthTextColor: lightOrDarkColor,
            indicatorColor: lightOrDarkColor,
            textDayFontFamily: "Roboto-Regular",
            textMonthFontFamily: "Roboto-Bold",
            textDayHeaderFontFamily: "Roboto-Medium",
          }}
        />
        {isLoading ? (
          <View style={styles.timelinePlaceholderContainer}>
            <ActivityIndicator
              color={Colors.primary.s800}
              animating={true}
              size={Sizing.x30}
              style={{
                margin: "auto",
              }}
            />
          </View>
        ) : (
          <TimelineList
            key={currentMonth}
            events={timeLineEvents}
            showNowIndicator
            scrollToNow
            initialTime={INITIAL_TIME}
          />
        )}
      </CalendarProvider>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  calendarContainer: {
    flex: 1,
  },
  timelinePlaceholderContainer: {
    flex: 1,
    backgroundColor: "white",
    justifyContent: "center",
  },
})
