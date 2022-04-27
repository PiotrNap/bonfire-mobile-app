import * as React from "react"
import {
  View,
  StyleSheet,
  LayoutRectangle,
  LayoutChangeEvent,
  Text,
  Animated,
  Easing,
} from "react-native"

import {
  appContext,
  eventCreationContext,
  myCalendarContext,
} from "contexts/contextApi"
import { Colors, Typography, Sizing, Outlines } from "styles/index"
import { MonthItem } from "./MonthItem"
import { CalendarHeader, Month } from "interfaces/myCalendarInterface"
import { monthsByName } from "common/types/calendarTypes"
import { WeekDayNames } from "./WeekDayNames"
import { CalendarTopNavigation } from "./navigation/calendarTopNavigation"
import { CalendarLegend } from "./CalendarLegend"
import { useCalendarEvents } from "lib/hooks/useCalendarEvents"

export interface MonthlyWrapperProps {
  isBookingCalendar?: boolean
  isNewEventCalendar?: boolean
  isRegularCalendar?: boolean
  customCallback?: () => Promise<void>
  secondCustomCallback?: (arg: string | null) => void
  initialEventsLoaded?: boolean
}

export const MonthlyWrapper = ({
  isBookingCalendar,
  isNewEventCalendar,
  isRegularCalendar,
  secondCustomCallback,
  initialEventsLoaded,
}: MonthlyWrapperProps) => {
  const {
    calendar,
    changeMonthHeader,
    calendarHeader,
    loadMyCalendar,
    updateCalendarMonth,
    setEvents,
  } = myCalendarContext()
  const { colorScheme } = appContext()
  const { selectedWeekDays } = eventCreationContext()
  const { fetchEvents } = useCalendarEvents()

  const [dimensions, setDimensions] = React.useState<LayoutRectangle | null>(
    null
  )
  const [currIndex, setCurrIndex] = React.useState<number>(1)
  const [monthsArray, setMonthsArray] = React.useState(calendar)
  const [direction, setDirection] = React.useState<"left" | "right" | null>(
    null
  )
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [initialHasLoaded, setInitialHasLoaded] = React.useState<boolean>(false)
  const [initialAnimationLoaded, setInitialAnimationLoaded] =
    React.useState<boolean>(false)
  const [_initialEventsLoaded, _setInitialEventsLoaded] =
    React.useState<boolean>(false)
  const [eventsLoading, setEventsLoading] = React.useState<boolean>(false)
  const [fetchedEventsFrom, setFetchedEventsFrom] = React.useState<{
    month: number
    year: number
    direction: "left" | "right"
  } | null>(null)
  const [tempEvents, setTempEvents] = React.useState<any>(null)

  var animatedOpacity = React.useRef(new Animated.Value(1)).current
  var animatedInitialOpacity = React.useRef(new Animated.Value(0)).current
  var animatedTranslateX = React.useRef(new Animated.Value(0)).current

  const isDarkMode = colorScheme === "dark"
  const isNotNextYear =
    calendar[currIndex + 1]?.year !== new Date().getFullYear()
  const isNotPreviousYear =
    calendar[currIndex - 1]?.year !== new Date().getFullYear()
  const prevMonth = calendar[currIndex - 1]?.name
  const prevYear = calendar[currIndex - 1]?.year
  const nextMonth = calendar[currIndex + 1]?.name
  const nextYear = calendar[currIndex + 1]?.year

  const startCalendarAnimation = (fadeOutPrevious: boolean) => {
    Animated.parallel([
      Animated.timing(animatedOpacity, {
        toValue: 0,
        duration: 80,
        useNativeDriver: true,
        easing: Easing.sin,
      }),
      Animated.timing(animatedTranslateX, {
        toValue: fadeOutPrevious ? 20 : -20,
        duration: 80,
        useNativeDriver: true,
        easing: Easing.sin,
      }),
    ]).start(() => {
      animatedTranslateX.setValue(0)
      animatedOpacity.setValue(1)
    })
  }

  const onPlaceholderPress = (direction: string) => {
    if (direction === "previous") onPreviousStartAnimation()
    if (direction === "next") onNextStartAnimation()
  }

  const WeekComponent = React.useCallback(() => {
    return <WeekDayNames isNewEventCalendar={isNewEventCalendar} />
  }, [monthsArray, selectedWeekDays])

  const CurrMonth = React.useCallback(
    ({ item }: { item: Month }) => (
      <Animated.View
        style={[
          styles.monthContainer,
          {
            opacity: !initialAnimationLoaded
              ? animatedInitialOpacity
              : animatedOpacity,
            transform: [
              {
                translateX: animatedTranslateX,
              },
            ],
            width: dimensions ? dimensions.width : 0,
            height: dimensions ? dimensions.height : 0,
          },
        ]}>
        <MonthItem
          days={item.days}
          year={item.year}
          month={item.name}
          firstDayName={item.firstDayName}
          numOfDays={item.numOfDays}
          name={item.name}
          dimensions={dimensions}
          onPlaceholderPress={onPlaceholderPress}
          isBookingCalendar={isBookingCalendar}
          isNewEventCalendar={isNewEventCalendar}
          secondCustomCallback={secondCustomCallback}
        />
      </Animated.View>
    ),
    [!!dimensions, initialAnimationLoaded]
  )

  const loadNewMonths = (nextMonths: boolean, month: number, year?: number) => {
    const calendarSetup = {
      nextMonths,
      month,
      year,
      isBookingCalendar,
      isRegularCalendar,
    }
    loadMyCalendar(calendarSetup)
  }

  const onPreviousStartAnimation = () => {
    if (isLoading) return
    setIsLoading(true)
    setDirection("left")

    const calendarHeader: CalendarHeader = {
      month: prevMonth,
      year: prevYear,
      numOfEvents: calendar[currIndex - 1]?.numOfEvents,
    }
    changeMonthHeader(calendarHeader)
    startCalendarAnimation(true)
    setCurrIndex(0)
    onPreviousLoadCalendar()
  }

  const onNextStartAnimation = () => {
    if (isLoading) return
    setIsLoading(true)
    setDirection("right")

    const calendarHeader = {
      month: nextMonth,
      year: nextYear,
      numOfEvents: calendar[currIndex + 1]?.numOfEvents,
    }

    changeMonthHeader(calendarHeader)
    startCalendarAnimation(false)
    setCurrIndex(2)
    onNextLoadCalendar()
  }

  const startInitialCalendarAnimation = () => {
    Animated.timing(animatedInitialOpacity, {
      toValue: 1,
      duration: 200,
      easing: Easing.sin,
      useNativeDriver: true,
    }).start(({ finished }) => finished && setInitialAnimationLoaded(true))
  }

  const onLayout = (event: LayoutChangeEvent) => {
    setDimensions(event.nativeEvent.layout)
    if (!direction && !initialAnimationLoaded) {
      startInitialCalendarAnimation()
    }
  }

  const onPreviousLoadCalendar = () => {
    // For the last month of current year
    if (calendarHeader.month === "January") {
      loadNewMonths(
        false,
        monthsByName[monthsArray[currIndex - 1].name],
        isNotPreviousYear ? calendar[currIndex - 1].year : undefined
      )
    } else if (isNotPreviousYear) {
      // for when the year isnt' the current one
      loadNewMonths(
        false,
        monthsByName[monthsArray[currIndex - 1].name],
        calendarHeader.year
      )
    } else {
      loadNewMonths(false, monthsByName[monthsArray[currIndex - 1].name])
    }
  }

  const onNextLoadCalendar = () => {
    // if the month is the first month of the next year, pass the next year
    // as last parameter
    if (calendarHeader.month === "December") {
      loadNewMonths(
        true,
        monthsByName[monthsArray[currIndex + 1].name],
        isNotNextYear ? calendar[currIndex + 1].year : undefined
      )
    } else if (isNotNextYear) {
      // for when the year isnt' the current one
      loadNewMonths(
        true,
        monthsByName[monthsArray[currIndex + 1].name],
        calendarHeader.year
      )
    } else {
      loadNewMonths(true, monthsByName[monthsArray[currIndex + 1].name])
    }
  }

  React.useEffect(() => {
    // if we've fetched new events but user has already
    // changed to another month, just return.
    if (!isRegularCalendar || isLoading || !fetchedEventsFrom) return

    const isNextMonth = fetchedEventsFrom.direction === "right"
    const index = isNextMonth ? currIndex + 1 : currIndex - 1
    const month = monthsByName[monthsArray[index].name]
    const year = monthsArray[currIndex].year
    const isSameMonth =
      fetchedEventsFrom.month === month && fetchedEventsFrom.year === year

    if (!isSameMonth) return

    const newCalendarSettings = {
      nextMonths: isNextMonth,
      month,
      year,
      isBookingCalendar,
      isRegularCalendar,
    }

    setEvents(tempEvents)
    setFetchedEventsFrom(null)
    updateCalendarMonth(newCalendarSettings)
  }, [tempEvents])

  React.useEffect(() => {
    if (eventsLoading && !fetchedEventsFrom && direction) {
      ;(async () => {
        const month = monthsByName[monthsArray[currIndex].name]
        const year = monthsArray[currIndex].year

        try {
          const _events = await fetchEvents(new Date(year, month), false)

          if (_events && _events.length) {
            // this will set our temporary events, and fire useeffect above
            // to check if they're from currently previewing month
            setDirection(null)
            setFetchedEventsFrom({ month, year, direction })
            setTempEvents(_events)
          }

          setEventsLoading(false)
        } catch (err) {
          return
        }
      })()
    }
  }, [eventsLoading])

  React.useEffect(() => {
    if (!initialHasLoaded) {
      setInitialHasLoaded(true)
      return setMonthsArray(calendar)
    }

    if (initialEventsLoaded && !_initialEventsLoaded) {
      setMonthsArray(calendar)
      return _setInitialEventsLoaded(true)
    }

    if (direction) {
      if (!isRegularCalendar) {
        setDirection(null)
      } else setEventsLoading(true)

      setIsLoading(false)
      setMonthsArray(calendar)
      setCurrIndex(1)
    } else if (!fetchedEventsFrom && tempEvents) {
      setMonthsArray(calendar)
      setTempEvents(null)
    }
  }, [calendar])

  // Do not pass inline functions as props, as they will be recreated
  // on each component re-render (and slowing down the app)
  return (
    <View style={{ alignItems: "center" }}>
      <View style={styles.container}>
        <View style={styles.headerContainer}>
          <View style={styles.header}>
            <Text
              style={
                colorScheme === "light"
                  ? styles.headerMonth_light
                  : styles.headerMonth_dark
              }>
              {calendarHeader.month}
            </Text>
            <Text
              style={
                colorScheme === "light"
                  ? styles.headerYear_light
                  : styles.headerYear_dark
              }>
              {calendarHeader.year}
            </Text>
          </View>
          <View style={styles.headerMonthNavigation}>
            <CalendarTopNavigation
              onPreviousPress={onPreviousStartAnimation}
              onNextPress={onNextStartAnimation}
              colorScheme={colorScheme}
              calendarHeader={calendarHeader}
              isBookingCalendar={isBookingCalendar}
              isNewEventCalendar={isNewEventCalendar}
            />
          </View>
        </View>
        <View
          style={[
            styles.calendar,
            { backgroundColor: isDarkMode ? Colors.primary.neutral : "white" },
          ]}>
          <WeekComponent />
          <View style={styles.calendarContainer} onLayout={onLayout}>
            {dimensions && calendar && (
              <CurrMonth item={monthsArray[currIndex]} />
            )}
          </View>
        </View>
      </View>
      {isRegularCalendar ||
        (isBookingCalendar && (
          <View style={styles.legendWrapper}>
            <CalendarLegend
              colorScheme={colorScheme}
              isBookingCalendar={isBookingCalendar}
              isRegularCalendar={isRegularCalendar}
            />
          </View>
        ))}
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    width: "90%",
    marginTop: Sizing.x10,
  },
  legendWrapper: {
    marginTop: Sizing.x10,
    marginLeft: Sizing.x5,
    alignSelf: "flex-start",
  },
  calendarContainer: {
    width: "100%",
    height: Sizing.x150,
    marginTop: Sizing.x7,
  },
  calendar: {
    minHeight: 200,
    width: "100%",
    marginTop: Sizing.x5,
    paddingVertical: Sizing.x10,
    paddingHorizontal: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.lifted,
  },
  headerContainer: {
    width: "100%",
    paddingHorizontal: Sizing.x5,
    flexDirection: "row",
    justifyContent: "space-between",
    marginBottom: Sizing.x5,
  },
  header: {
    width: "60%",
    flexDirection: "row",
    marginLeft: Sizing.x8,
    alignItems: "baseline",
  },
  headerMonthNavigation: {
    flexDirection: "row",
    width: Sizing.x80,
    height: "100%",
    marginRight: Sizing.x8,
    justifyContent: "space-between",
    alignItems: "center",
  },
  headerMonth_light: {
    ...Typography.header.x55,
    color: Colors.primary.s600,
    paddingRight: 5,
  },
  headerMonth_dark: {
    ...Typography.header.x55,
    color: Colors.primary.neutral,
    paddingRight: 5,
  },
  headerYear_light: {
    ...Typography.header.x35,
    color: Colors.primary.s300,
    paddingRight: 5,
    lineHeight: 30,
  },
  headerYear_dark: {
    ...Typography.header.x35,
    color: Colors.primary.neutral,
    paddingRight: 5,
    lineHeight: 30,
  },
  monthContainer: {
    flexWrap: "wrap",
    flexDirection: "row",
  },
})
