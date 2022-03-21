import * as React from "react";
import {
  View,
  LayoutChangeEvent,
  LayoutRectangle,
  StyleSheet,
  SectionList,
  Text,
  VirtualizedList,
} from "react-native";
import { CalendarEventsDetail } from "./CalendarEventsDetail";

import { appContext, myCalendarContext } from "contexts/contextApi";
import { getDate, getYear } from "lib/utils";
import { Sizing, Colors, Outlines, Typography } from "styles/index";
import { Event } from "common/interfaces/myCalendarInterface";
import { CalendarEventsListHeader } from "./CalendarEventsListHeader";
import { months, monthsByName } from "common/types/calendarTypes";

export interface CalendarEventsListProps {
  isHomeScreen?: boolean;
  isBookingCalendar?: boolean;
  isRegularCalendar?: boolean;
  currentSelectedDay?: Date | null;
}

export const CalendarEventsList = ({
  isHomeScreen,
  isBookingCalendar,
  isRegularCalendar,
  currentSelectedDay,
}: CalendarEventsListProps) => {
  const { events, calendarHeader } = myCalendarContext();
  const { colorScheme, accountType } = appContext();
  const [dimensions, setDimensions] = React.useState<LayoutRectangle | null>(
    null
  );
  // console.log(JSON.stringify(events, null, 4));
  // console.log(currentSelectedDay);
  const [highlightedDay, setHighlightedDay] = React.useState<any>({
    listSection: "",
    index: null,
  });
  const [currMonthEvents, setCurrMonthEvents] = React.useState<any[]>([]);

  // console.log("current selected day is :", currentSelectedDay);

  const renderItem = ({ item, index, section }: any) => {
    const { title, description, fromTime, toTime, participants, organizer } =
      item;

    return <Text>{item.id}</Text>;

    // return (
    //   <CalendarEventsDetail
    //     key={`${item.fromTime}_${item.toTime}`}
    //     index={index}
    //     title={title}
    //     description={description}
    //     fromTime={fromTime}
    //     toTime={toTime}
    //     participants={participants}
    //     organizer={organizer}
    //     listLength={section.data.length}
    //     listSection={section.title}
    //     highlightedDay={highlightedDay}
    //     setHighlightedDay={setHighlightedDay}
    //   />
    // );
  };

  const keyExtractor = (item: any, index: number) =>
    `${index}_${item.fromTime}_${item.toTime}`;

  const onLayout = (event: LayoutChangeEvent) => {
    setDimensions(event.nativeEvent.layout);
  };

  const getCurrMonthEvents = React.useCallback((): {
    monthlyEvents?: Event[];
    dayEvents?: Event[];
  } => {
    var monthlyEvents: Event[] = [];
    var dayEvents: Event[] = [];

    // merge two arrays,
    // based on the event type ('booked event' or 'scheduled event') display the right color/info,

    if (events) {
      for (let scheduledYear of events) {
        if (scheduledYear.year === getYear()) {
          if (scheduledYear.months) {
            var monthObj = scheduledYear.months.find((obj) => {
              if (isHomeScreen && accountType === "attendee") {
                return obj.month === months[new Date().getMonth()];
              }
              return obj.month === calendarHeader.month;
            });

            if (monthObj != null) {
              monthObj.days.forEach((day: any) =>
                day.events.forEach((evt: Event) => {
                  if (isHomeScreen && day.day === new Date().getDate()) {
                    dayEvents.push(evt);
                  } else if (day.day === getDate()) {
                    dayEvents.push(evt);
                  } else {
                    monthlyEvents.push(evt);
                  }
                })
              );
            }
          }
        }
      }
    }

    return { monthlyEvents, dayEvents };
  }, [calendarHeader.month, events]);

  const getSections = () => {
    const { monthlyEvents, dayEvents } = getCurrMonthEvents();
    const sections: any[] = [];

    if (dayEvents.length) {
      sections.push({ title: "Today", data: [...dayEvents] });
    }
    if (monthlyEvents.length) {
      sections.push({
        title: "This month",
        data: [...monthlyEvents],
      });
    }

    return sections;
  };

  // console.log("data is ", JSON.stringify(data(), null, 4));

  const sectionHeader = ({ section }: any) => {
    const { title } = section;

    return (
      <View style={styles.sectionHeaderWrapper}>
        <Text
          style={
            colorScheme === "light"
              ? styles.sectionHeader_light
              : styles.sectionHeader_dark
          }>
          {title}
        </Text>
      </View>
    );
  };
  const numOfEvents = React.useMemo(() => {
    if (!events) return 0;

    const uniqueEvents: Array<String> = [];
    const yearToSearch = calendarHeader.year;
    const monthToSearch = calendarHeader.month;

    if (currentSelectedDay) {
      let dayToSearch = new Date(currentSelectedDay).getDate();

      events
        .find((obj) => obj.year === yearToSearch)
        ?.months.find((obj) => obj.month === monthToSearch)
        ?.days.find((obj) => obj.day == dayToSearch)
        ?.events.forEach(
          (event) =>
            !uniqueEvents.includes(event.id) && uniqueEvents.push(event.id)
        );
    } else {
      events
        .find((obj) => obj.year === yearToSearch)
        ?.months.find((obj) => obj.month === monthToSearch)
        ?.days.forEach((obj) =>
          obj?.events.forEach(
            (event) =>
              !uniqueEvents.includes(event.id) && uniqueEvents.push(event.id)
          )
        );
    }
    return uniqueEvents.length;
  }, [currentSelectedDay, calendarHeader]);

  const getItemCount = (data: any) => data.length;
  const getItem = (data: any, index: number) => ({
    id: Math.random().toString(12).substring(0),
    ...data,
  });
  // console.log(JSON.stringify(getSections(), null, 4));

  return (
    <View style={styles.eventsHolder} onLayout={onLayout}>
      {numOfEvents ? (
        <CalendarEventsListHeader numOfEvents={numOfEvents} />
      ) : (
        <></>
      )}
      {(isBookingCalendar || isHomeScreen) &&
      getSections().length > 0 &&
      !isRegularCalendar ? (
        <SectionList
          contentContainerStyle={[
            {
              width: dimensions ? dimensions.width : "100%",
              paddingBottom: Sizing.x5,
            },
          ]}
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
          contentContainerStyle={[
            {
              width: dimensions ? dimensions.width : "100%",
              paddingBottom: Sizing.x5,
            },
          ]}
          getItem={getItem}
          getItemCount={getItemCount}
          data={
            currentSelectedDay !== null
              ? getCurrMonthEvents().dayEvents
              : getCurrMonthEvents().monthlyEvents
          }
          renderItem={renderItem}
          keyExtractor={keyExtractor}
          scrollEventThrottle={500}
          maxToRenderPerBatch={5}
          updateCellsBatchingPeriod={5}
          progressViewOffset={15}
          showsVerticalScrollIndicator={false}
        />
      )}
    </View>
  );
};

const styles = StyleSheet.create({
  eventsHolder: {
    flex: 1,
    width: "95%",
    alignItems: "center",
    borderRadius: Outlines.borderRadius.small,
  },
  sectionHeaderWrapper: {
    marginVertical: Sizing.x7,
  },
  sectionHeader_light: {
    width: "50%",
    alignSelf: "baseline",
    marginLeft: Sizing.x20,
    ...Typography.subHeader.x30,
    color: Colors.primary.s600,
  },
  sectionHeader_dark: {
    width: "50%",
    marginLeft: Sizing.x20,
    ...Typography.header.x30,
    color: Colors.primary.neutral,
  },
});
