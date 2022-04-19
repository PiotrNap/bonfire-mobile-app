import * as React from "react"
import { FlatList } from "react-native"

import {
  appContext,
  bookingContext,
  eventCreationContext,
} from "contexts/contextApi"
import { EventConfirmationDetail } from "./EventConfirmationDetail"
import {
  AdaIcon,
  CalendarIcon,
  ColorsPalleteIcon,
  DescriptionIcon,
  PlaceholderIcon,
  PresentationIcon,
  TimeIcon,
  UserIcon,
} from "assets/icons"
import { Colors, Sizing } from "styles/index"
import { months, weekDays } from "common/types/calendarTypes"
import { SectionDetail } from "common/interfaces/bookingInterface"
import {
  getDate,
  getDay,
  getDigitalLocaleTime,
  getMonth,
  getTimeSpanLength,
} from "lib/utils"
import { useAuthCredentials } from "lib/hooks/useAuthCredentials"

export const EventConfirmationDetails = ({
  isNewEvent = false,
  organizerEvent,
}: any) => {
  const { colorScheme } = appContext()
  const {
    duration = null,
    durationCost = null,
    pickedDate = null,
    previewingEvent,
  } = bookingContext()
  const {
    textContent,
    hourlyRate: eventHourlyRate,
    selectedDays,
    imageURI,
    eventCardColor,
    eventTitleColor,
  } = eventCreationContext()
  const credentials = useAuthCredentials()

  var selectedDaysArr: number[] = []
  var fromDate, toDate

  if (isNewEvent) {
    selectedDaysArr = Object.values(selectedDays as any)
    fromDate = Math.min(...selectedDaysArr)
    toDate = Math.max(...selectedDaysArr)
  }

  const isLightMode = colorScheme === "light"

  const iconStyles = {
    stroke: isLightMode ? Colors.primary.s600 : Colors.primary.s200,
    strokeWidth: 1.8,
    width: 24,
    height: 24,
    marginRight: Sizing.x5,
  }

  const sectionsIcons = {
    presentation: <PresentationIcon {...iconStyles} />,
    description: <DescriptionIcon {...iconStyles} />,
    user: <UserIcon {...iconStyles} />,
    calendar: <CalendarIcon {...iconStyles} />,
    time: <TimeIcon {...iconStyles} />,
    ada: <AdaIcon {...iconStyles} />,
    placeholder: <PlaceholderIcon {...iconStyles} />,
    colorsPallete: <ColorsPalleteIcon {...iconStyles} />,
  }

  const organizerEventSections: any[] = [
    organizerEvent?.title && {
      label: "Title",
      lineContent: {
        content: organizerEvent?.title,
        icon: sectionsIcons.presentation,
      },
    },

    organizerEvent?.description && {
      label: "Description",
      lineContent: {
        content: organizerEvent?.description,
        icon: sectionsIcons.description,
      },
    },
    organizerEvent?.fromDate && {
      label: "Date & time",
      lineContent: [
        {
          content: `Start: ${weekDays[getDay(organizerEvent?.fromDate)]} - ${
            months[getMonth(organizerEvent?.fromDate)]
          } ${getDate(organizerEvent?.fromDate)}`,
          icon: sectionsIcons.calendar,
        },
        {
          content: `End: ${weekDays[getDay(organizerEvent?.toDate)]} - ${
            months[getMonth(organizerEvent?.toDate)]
          } ${getDate(organizerEvent?.toDate)}`,
          icon: sectionsIcons.calendar,
        },
      ],
    },
    {
      label: "Hourly Rate",
      lineContent: {
        //if event has hourly rate of 0 take the rate from user info
        //stored on device's encrypted storage
        content: `${organizerEvent?.hourlyRate ?? credentials?.hourlyRate}`,
        icon: sectionsIcons.ada,
      },
    },
  ]

  const newEventSections: any[] = [
    textContent?.title && {
      label: "Title",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "New Event Description",
      },
      lineContent: {
        content: textContent.title,
        icon: sectionsIcons.presentation,
      },
    },
    textContent?.description && {
      label: "Description",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "New Event Description",
      },
      lineContent: {
        content: textContent.description,
        icon: sectionsIcons.description,
      },
    },
    selectedDays && {
      label: "Date & time",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "Available Days Selection",
      },
      lineContent: [
        {
          content: `Start: ${weekDays[getDay(fromDate)]} - ${
            months[getMonth(fromDate)]
          } ${getDate(fromDate)}`,
          icon: sectionsIcons.calendar,
        },
        {
          content: `End: ${weekDays[getDay(toDate)]} - ${
            months[getMonth(toDate)]
          } ${getDate(toDate)}`,
          icon: sectionsIcons.calendar,
        },
      ],
    },
    {
      label: "Hourly Rate",
      lineContent: {
        content: `${eventHourlyRate}`,
        icon: sectionsIcons.ada,
      },
    },
    {
      label: "Event card",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "Event Card Customization",
      },
      lineContent: [
        {
          content: imageURI,
          icon: sectionsIcons.placeholder,
        },
        eventCardColor !== "transparent" && {
          content: `${eventCardColor}`,
          icon: sectionsIcons.colorsPallete,
        },
        eventTitleColor !== "transparent" && {
          content: `${eventTitleColor}`,
          icon: sectionsIcons.colorsPallete,
        },
      ],
    },
  ]

  const bookingEventSections: SectionDetail[] = [
    previewingEvent?.title && {
      label: "Event",
      lineContent: {
        content: previewingEvent.title,
        icon: sectionsIcons.presentation,
      },
    },
    previewingEvent?.organizerAlias && {
      label: "Organizer",
      lineContent: {
        content: previewingEvent.organizerAlias,
        icon: sectionsIcons.user,
      },
    },
    pickedDate &&
      duration && {
        label: "Date & time",
        callbackFn: {
          label: "Edit",
          callbackFnScreen: "Available Event Days Selection",
        },
        lineContent: [
          {
            content: `${weekDays[getDay(pickedDate)]} - ${
              months[getMonth(pickedDate)]
            } ${getDate(pickedDate)}`,
            icon: sectionsIcons.calendar,
          },
          {
            content: `${getDigitalLocaleTime(
              pickedDate
            )} - ${getDigitalLocaleTime(
              new Date(pickedDate).getTime() + duration
            )}`,
            icon: sectionsIcons.time,
          },
        ],
      },
    duration && {
      label: "Reservation time",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "Duration Choice",
      },
      lineContent: {
        content: getTimeSpanLength(duration),
      },
    },
    durationCost && {
      label: "Total amount",
      lineContent: {
        content: `${durationCost}`,
        icon: sectionsIcons.ada,
      },
    },
  ]

  const isLastItem = (index: number) =>
    isNewEvent
      ? index === newEventSections.length - 1
      : organizerEvent
      ? index === organizerEventSections.length - 1
      : index === bookingEventSections.length - 1

  const renderSections = ({
    item,
    index,
  }: {
    item: SectionDetail
    index: number
  }) => (
    <EventConfirmationDetail
      key={index}
      label={item.label}
      lineContent={item.lineContent}
      callbackFn={item.callbackFn}
      isLastItem={isLastItem(index)}
    />
  )

  const keyExtractor = (item: any, index: number) => `${item.label}_${index}`

  return (
    <FlatList
      data={
        isNewEvent
          ? newEventSections
          : organizerEvent
          ? organizerEventSections
          : bookingEventSections
      }
      renderItem={renderSections}
      keyExtractor={keyExtractor}
    />
  )
}
