import * as React from "react"
import { CalendarEventsList, MonthlyWrapper } from "components/calendar"
import { appContext, eventCreationContext, myCalendarContext } from "contexts/contextApi"
import { ActivityIndicator, StyleSheet, View } from "react-native"
import { Colors, Sizing } from "styles/index"
import { useNavigation } from "@react-navigation/native"
import { ProfileContext } from "contexts/profileContext"
import { useCalendarEvents } from "lib/hooks/useCalendarEvents"
import { monthsByName } from "common/types/calendarTypes"
import { Events, EventsMonth } from "common/interfaces/myCalendarInterface"
import { SmallButton } from "components/buttons/smallButton"
import { PlusIcon } from "assets/icons"
import { EventCreationParamList } from "common/types/navigationTypes"
import { ValueOf } from "react-native-gesture-handler/lib/typescript/typeUtils"

export interface CalendarProps {
  isBookingCalendar?: boolean
  isHomeScreen?: boolean
  isRegularCalendar?: boolean
  navigateCb: (
    name: keyof EventCreationParamList,
    params: ValueOf<EventCreationParamList>
  ) => void
}

export const Calendar = ({
  isBookingCalendar,
  isHomeScreen,
  isRegularCalendar,
  navigateCb,
}: CalendarProps) => {
  const { colorScheme, appBgColor } = appContext()
  const { resetEventCreationState } = eventCreationContext()
  const { id } = React.useContext(ProfileContext)
  const { events, calendarHeader } = myCalendarContext()
  const { getEvents, loadingEvents } = useCalendarEvents(id)

  const [currentSelectedDay, setCurrentSelectedDay] = React.useState<
    undefined | string | null
  >(null)

  const isLightMode = colorScheme === "light"
  const navigation = useNavigation()

  React.useEffect(() => {
    if (!id) return
    getEvents(true, new Date(calendarHeader.year, monthsByName[calendarHeader.month]))

    const subscribe = () => {
      navigation.addListener("blur", () => {})
      navigation.addListener("focus", async () => {
        getEvents(
          false,
          new Date(calendarHeader.year, monthsByName[calendarHeader.month])
        )
      })
    }

    const unsubscribe = () => {
      navigation.removeListener("blur", () => {})
      navigation.removeListener("focus", () => {})
    }

    subscribe()

    return unsubscribe
  }, [id])

  const onAddEventPress = () => {
    resetEventCreationState()
    navigation.navigate("New Event Description")
  }
  const onSelectedDayChange = (day: string | null) => setCurrentSelectedDay(day)
  const isMonthWithEvents = !!events?.map((eventsYear: Events) =>
    eventsYear.months.find(
      (eventsMonth: EventsMonth) => eventsMonth.month === calendarHeader.month
    )
  )[0]

  return (
    <View style={{ flex: 1, backgroundColor: appBgColor }}>
      <MonthlyWrapper
        secondCustomCallback={onSelectedDayChange}
        isRegularCalendar={isRegularCalendar}
        initialEventsLoaded={!!loadingEvents}
      />
      {(!isBookingCalendar || isRegularCalendar) &&
        (loadingEvents ? (
          <View style={styles.buttonWrapper}>
            <ActivityIndicator
              animating={true}
              color={isLightMode ? Colors.primary.s800 : Colors.primary.neutral}
              size="large"
              style={{ paddingTop: Sizing.x35 }}
            />
          </View>
        ) : isMonthWithEvents ? (
          <CalendarEventsList
            currentSelectedDay={currentSelectedDay}
            isRegularCalendar={isRegularCalendar}
            isHomeScreen={isHomeScreen}
            navigateCb={navigateCb}
          />
        ) : (
          <View style={styles.buttonWrapper}>
            <SmallButton
              onPress={onAddEventPress}
              icon={
                <PlusIcon
                  color={Colors.primary.neutral}
                  width={Sizing.x20}
                  height={Sizing.x20}
                  strokeWidth={4}
                />
              }
              title="Create Event"
            />
          </View>
        ))}
      {/*
      {isMonthWithEvents && !loadingEvents && (
        <View style={styles.cornerButtonWrapper}>
          <RoundedButton
            onPress={onAddEventPress}
            icon={
              <PlusIcon
                color={Colors.primary.neutral}
                width={Sizing.x40}
                height={Sizing.x40}
                strokeWidth={2}
              />
            }
          />
        </View>
      )}
      */}
    </View>
  )
}

const styles = StyleSheet.create({
  buttonWrapper: {
    flex: 1,
    justifyContent: "center",
    alignItems: "center",
  },
  cornerButtonWrapper: {
    position: "absolute",
    bottom: Sizing.x15,
    right: Sizing.x15,
  },
})
