import * as React from "react"
import {
  View,
  StyleSheet,
  SectionList,
  Text,
  VirtualizedList,
} from "react-native"
import { CalendarEventsDetail } from "./CalendarEventsDetail"

import { appContext, myCalendarContext } from "contexts/contextApi"
import { getDate, getYear } from "lib/utils"
import { Sizing, Colors, Outlines, Typography } from "styles/index"
import {
  CalendarSectionTitles,
  Event,
} from "common/interfaces/myCalendarInterface"
import { CalendarEventsListHeader } from "./CalendarEventsListHeader"
import { months, monthsByName } from "common/types/calendarTypes"

export interface CalendarEventsListProps {
  isHomeScreen?: boolean
  isRegularCalendar?: boolean
  currentSelectedDay?: string | null
}

export const CalendarEventsList = ({
  isHomeScreen,
  isRegularCalendar,
  currentSelectedDay,
}: CalendarEventsListProps) => {
  const { events, calendarHeader } = myCalendarContext()
  const { colorScheme, accountType, userSettings } = appContext()
  const [highlightedDay, setHighlightedDay] = React.useState<any>({
    listSection: "",
    index: null,
  })
  const isCurrMonth: boolean =
    new Date().getMonth() === monthsByName[calendarHeader.month] &&
    new Date().getFullYear() === calendarHeader.year
  const isSelectedDayAtViewMonth =
    currentSelectedDay &&
    new Date(currentSelectedDay).getMonth() ===
      monthsByName[calendarHeader.month] &&
    new Date(currentSelectedDay).getFullYear() === calendarHeader.year

  const renderItem = React.useCallback(
    ({ item, index, section }: any) => (
      <CalendarEventsDetail
        key={item.id}
        index={index}
        highlightedDay={highlightedDay}
        setHighlightedDay={setHighlightedDay}
        listLength={section?.data.length}
        listSection={section?.title}
        currentSelectedDay={currentSelectedDay}
        {...item}
      />
    ),
    [events, highlightedDay, currentSelectedDay]
  )

  const keyExtractor = (item: any, index: number) =>
    `${index}_${item.fromTime}_${item.toTime}`

  const getMonthEvents = React.useCallback((): {
    monthlyEvents?: Event[]
    dayEvents?: Event[]
    todayEvents?: Event[]
  } => {
    var monthlyEvents: Event[] = []
    var dayEvents: Event[] = []
    var todayEvents: Event[] = []
    const showPastEvents = userSettings?.showPastCalendarEvents || false

    if (events) {
      for (let eventsYear of events) {
        if (eventsYear.year === getYear()) {
          if (eventsYear.months) {
            var monthObj = eventsYear.months.find((obj) => {
              if (isHomeScreen && accountType === "attendee") {
                return obj.month === months[new Date().getMonth()]
              }
              return obj.month === calendarHeader.month
            })

            if (monthObj != null) {
              monthObj.days.forEach((day: any) => {
                // if (!showPastEvents && day.day < getDate()) return
                day.events.forEach((evt: Event) => {
                  if (isHomeScreen && day.day === getDate(currentSelectedDay)) {
                    dayEvents.push(evt)
                  } else if (day.day === getDate()) {
                    todayEvents.push(evt)
                  } else if (
                    monthlyEvents.findIndex((mEvt) => mEvt.id === evt.id) === -1
                  ) {
                    monthlyEvents.push(evt)
                  }
                })
              })
            }
          }
        }
      }
    }
    return { monthlyEvents, dayEvents, todayEvents }
  }, [calendarHeader.month, events, currentSelectedDay])

  const getSections = () => {
    const { monthlyEvents, dayEvents } = getMonthEvents()
    const sections: { title: CalendarSectionTitles; data: any[] }[] = []

    if (dayEvents?.length) {
      sections.push({
        title: CalendarSectionTitles.today,
        data: [...dayEvents],
      })
    }
    if (monthlyEvents?.length) {
      sections.push({
        title: CalendarSectionTitles.thisMonth,
        data: [...monthlyEvents],
      })
    }
    return sections
  }

  const sectionHeader = ({ section }: any) => {
    const { title } = section

    return (
      <View style={styles.sectionHeaderWrapper}>
        <Text
          style={[
            styles.sectionHeader,
            {
              color:
                colorScheme === "light"
                  ? Colors.primary.s600
                  : Colors.primary.neutral,
            },
          ]}>
          {title}
        </Text>
      </View>
    )
  }
  const numOfEvents = React.useMemo(() => {
    if (!events) return 0

    const uniqueEvents: Array<String> = []
    const yearToSearch = calendarHeader.year
    const monthToSearch = calendarHeader.month

    if (currentSelectedDay) {
      let dayToSearch = new Date(currentSelectedDay).getDate()

      events
        .find((obj) => obj.year === yearToSearch)
        ?.months.find((obj) => obj.month === monthToSearch)
        ?.days.find((obj) => obj.day == dayToSearch)
        ?.events.forEach(
          (event) =>
            !uniqueEvents.includes(event.id) && uniqueEvents.push(event.id)
        )
    } else {
      events
        .find((obj) => obj.year === yearToSearch)
        ?.months.find((obj) => obj.month === monthToSearch)
        ?.days.forEach((obj) =>
          obj?.events.forEach(
            (event) =>
              !uniqueEvents.includes(event.id) && uniqueEvents.push(event.id)
          )
        )
    }
    return uniqueEvents.length
  }, [currentSelectedDay, calendarHeader])

  const getItemCount = (data: any) => data.length
  const getItem = (data: any, index: number) => ({
    id: Math.random().toString(12).substring(0),
    ...data[index],
  })

  return (
    <View style={styles.eventsHolder}>
      {numOfEvents && accountType === "attendee" ? (
        <CalendarEventsListHeader numOfEvents={numOfEvents} />
      ) : (
        <></>
      )}
      {isCurrMonth &&
      getSections().length > 0 &&
      isRegularCalendar &&
      !currentSelectedDay ? (
        <SectionList
          contentContainerStyle={[styles.list, styles.sectionList]}
          renderItem={renderItem}
          keyExtractor={keyExtractor}
          scrollEventThrottle={500}
          maxToRenderPerBatch={5}
          updateCellsBatchingPeriod={5}
          progressViewOffset={15}
          sections={getSections()}
          renderSectionHeader={sectionHeader}
          stickySectionHeadersEnabled={false}
          showsVerticalScrollIndicator={false}
        />
      ) : (
        <VirtualizedList
          contentContainerStyle={[styles.list, styles.virtualizedList]}
          getItem={getItem}
          getItemCount={getItemCount}
          data={
            currentSelectedDay
              ? getMonthEvents().dayEvents
              : getMonthEvents().monthlyEvents
          }
          renderItem={renderItem}
          keyExtractor={keyExtractor}
          scrollEventThrottle={500}
          maxToRenderPerBatch={5}
          updateCellsBatchingPeriod={5}
          progressViewOffset={15}
          initialNumToRender={10}
          showsVerticalScrollIndicator={false}
        />
      )}
    </View>
  )
}

const styles = StyleSheet.create({
  eventsHolder: {
    flex: 1,
    alignItems: "center",
    borderRadius: Outlines.borderRadius.small,
    marginVertical: Sizing.x5,
  },
  list: {
    minWidth: "95%",
    maxWidth: "95%",
  },
  sectionList: {
    paddingBottom: Sizing.x5,
  },
  virtualizedList: {
    paddingTop: Sizing.x20,
    paddingBottom: Sizing.x5,
  },
  sectionHeaderWrapper: {
    marginVertical: Sizing.x7,
  },
  sectionHeader: {
    width: "50%",
    marginLeft: Sizing.x20,
    ...Typography.header.x25,
  },
})
