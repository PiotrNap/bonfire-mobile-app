import { useFocusEffect } from "@react-navigation/native"
import { Events } from "Api/Events"
import { Users } from "Api/Users"
import { ProfileContext } from "contexts/profileContext"
import { getCalendarDate, showErrorToast } from "lib/helpers"
import { useCalendarEvents } from "lib/hooks/useCalendarEvents"
import { useEventsPagination } from "lib/hooks/useEventsPagination"
import * as React from "react"
import {
  CalendarProvider,
  ExpandableCalendar,
  TimelineEventProps,
  TimelineList,
} from "react-native-calendars"
import { MarkedDates } from "react-native-calendars/src/types"

const INITIAL_TIME = { hour: new Date().getHours(), minutes: new Date().getMinutes() }

// Questions :
// - how am I going to convert the events current format to this calendar?
export const MyCalendarScreen = ({ navigation }: any) => {
  const { id } = React.useContext(ProfileContext)
  // const [timeLine,setEvents] = React.useState<TimelineEventProps[]>

  const fetchCalendarData = async () => {
    try {
      let calendarData = await Users.getUserCalendarEvents(id)
      if (!calendarData) throw new Error("Unable to get your calander data")

      const { bookedSlots, scheduledSlots, events } = calendarData
      let marked = {},
        timeLineEvents: TimelineEventProps[] = []

      //       events.map(event => {

      //       })
      //@TODO when there are events booked make sure they show on the calendar & timeline
      // let bookings = await Events.getBookingByQuery({ user_id: id })
      console.log(JSON.stringify(events, null, 4))
    } catch (e) {
      showErrorToast(e)
    }
  }

  useFocusEffect(
    React.useCallback(() => {
      fetchCalendarData()
    }, [id])
  )

  const [marked, setMarked] = React.useState<MarkedDates>({
    "2023-11-19": {
      dots: [
        { key: "booking", color: "blue", selectedDotColor: "blue" },
        { key: "event", color: "red", selectedDotColor: "red" },
      ],
      // selected: true,
    },
    // "2023-11-18": { marked: true },
    // "2023-11-17": { marked: true },
  })
  const currentDate = getCalendarDate()
  const onDateChanged = (date) => console.log(date)
  const onMonthChange = (date) => console.log(date)
  return (
    <CalendarProvider
      date={currentDate}
      onDateChanged={onDateChanged}
      onMonthChange={onMonthChange}
      showTodayButton
      disabledOpacity={0.6}

      // numberOfDays={3}
    >
      <ExpandableCalendar
        firstDay={1}
        // leftArrowImageSource={require("../img/previous.png")}
        // rightArrowImageSource={require("../img/next.png")}
        markedDates={marked}
        markingType={"multi-dot"}
      />
      {/*
      <TimelineList
        // events={eventsByDate}
        // timelineProps={timelineProps}
        showNowIndicator
        // scrollToNow
        scrollToFirst
        initialTime={INITIAL_TIME}
      />*/}
    </CalendarProvider>
  )
}
