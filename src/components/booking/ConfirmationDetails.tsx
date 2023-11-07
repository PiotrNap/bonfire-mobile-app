import * as React from "react"
import { FlatList } from "react-native"

import { appContext, bookingContext, eventCreationContext } from "contexts/contextApi"
import { ConfirmationDetail } from "./ConfirmationDetail"
import {
  AdaIcon,
  CalendarIcon,
  ColorsPalleteIcon,
  DescriptionIcon,
  FontIcon,
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
  isSameDay,
} from "lib/utils"
import { AnyObject } from "yup/lib/types"
import tz from "react-native-timezone"

interface ConfirmationDetails {
  isNewEvent: boolean
  isCalendarEventPreview: boolean
  organizerEvent?: AnyObject
  bookedEvent?: AnyObject
}

export const ConfirmationDetails = ({
  isNewEvent = false,
  isCalendarEventPreview,
  organizerEvent,
  bookedEvent,
}: any) => {
  const { colorScheme } = appContext()
  var {
    duration = null,
    durationCost = null,
    pickedDate = null,
    previewingEvent: bookingPreviewingEvent,
  } = bookingContext()
  const {
    textContent,
    hourlyRate: eventHourlyRate,
    selectedDays,
    imageURI,
    eventCardColor,
    eventTitleColor,
  } = eventCreationContext()
  const previewingEvent = bookingPreviewingEvent || bookedEvent
  const [timeZone, setTimeZone] = React.useState<any>("")
  var selectedDaysArr: number[] = []
  var fromDate, toDate

  if (isNewEvent) {
    selectedDaysArr = Object.values(selectedDays as any)
    fromDate = Math.min(...selectedDaysArr)
    toDate = Math.max(...selectedDaysArr)
  }

  React.useEffect(() => {
    ;(async () => {
      let _timeZone = await tz.getTimeZone()
      setTimeZone(_timeZone)
    })()
  }, [])

  const isLightMode = colorScheme === "light"

  const iconStyles = {
    stroke: isLightMode ? Colors.primary.s600 : Colors.primary.s200,
    strokeWidth: 1.8,
    width: Sizing.x25,
    height: Sizing.x25,
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
    font: <FontIcon {...iconStyles} />,
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
      label: "Date & Time",
      lineContent: !isSameDay(organizerEvent?.fromDate, organizerEvent?.toDate)
        ? [
            {
              content: `Starts: ${weekDays[getDay(organizerEvent?.fromDate)]} - ${
                months[getMonth(organizerEvent?.fromDate)]
              } ${getDate(organizerEvent?.fromDate)}`,
              icon: sectionsIcons.calendar,
            },
            {
              content: `Ends: ${weekDays[getDay(organizerEvent?.toDate)]} - ${
                months[getMonth(organizerEvent?.toDate)]
              } ${getDate(organizerEvent?.toDate)}`,
              icon: sectionsIcons.calendar,
            },
          ]
        : [
            {
              content: `${new Date(organizerEvent?.fromDate).toLocaleString()}`,
              icon: sectionsIcons.calendar,
            },
          ],
    },
    organizerEvent?.availableAt && {
      label: "Date & Time",
      lineContent: [
        {
          content: `${weekDays[getDay(organizerEvent?.availableAt)]} - ${
            months[getMonth(organizerEvent?.availableAt)]
          } ${getDate(organizerEvent?.availableAt)}`,
          icon: sectionsIcons.calendar,
        },
        organizerEvent?.fromTimeSlot &&
          organizerEvent?.toTimeSlot && {
            content: `${getDigitalLocaleTime(
              organizerEvent?.fromTimeSlot
            )} - ${getDigitalLocaleTime(
              new Date(organizerEvent?.toTimeSlot).getTime()
            )} ${timeZone}`,
            icon: sectionsIcons.time,
          },
      ],
    },
    (organizerEvent?.hourlyRate.ada || organizerEvent?.hourlyRate.gimbals) && {
      label: "Hourly Rate",
      lineContent: [
        organizerEvent?.hourlyRate.ada && {
          content: `${organizerEvent?.hourlyRate.ada}`,
          icon: sectionsIcons.ada,
        },
        organizerEvent?.hourlyRate.gimbals && {
          content: `${organizerEvent?.hourlyRate.gimbals}`,
          icon: sectionsIcons.ada,
        },
      ],
    },
  ].filter((s) => !!s)

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
    selectedDays &&
      fromDate &&
      toDate && {
        label: "Date & Time",
        callbackFn: {
          label: "Edit",
          callbackFnScreen: "Available Days Selection",
        },
        lineContent: !isSameDay(fromDate, toDate)
          ? [
              {
                content: `Starts: ${new Date(fromDate).toLocaleString()}`,
                icon: sectionsIcons.calendar,
              },
              {
                content: `Ends: ${new Date(toDate).toLocaleString()}`,
                icon: sectionsIcons.calendar,
              },
            ]
          : [
              {
                content: `${new Date(fromDate).toLocaleString()}`,
                icon: sectionsIcons.calendar,
              },
            ],
      },
    {
      label: "Hourly Rate",
      lineContent: [
        eventHourlyRate.ada && {
          content: `${eventHourlyRate.ada}`,
          icon: sectionsIcons.ada,
        },
        eventHourlyRate.gimbals && {
          content: `${eventHourlyRate.gimbals}`,
          icon: sectionsIcons.ada,
        },
      ],
    },
    eventCardColor !== "transparent" && {
      label: "Event Card",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "Event Card Customization",
      },
      lineContent: [
        !!imageURI && {
          content: imageURI,
          icon: sectionsIcons.placeholder,
        },
        eventCardColor !== "transparent" && {
          content: `${eventCardColor}`,
          icon: sectionsIcons.colorsPallete,
        },
        eventTitleColor !== "transparent" && {
          content: `${eventTitleColor}`,
          icon: sectionsIcons.font,
        },
      ],
    },
  ].filter((s) => !!s)
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
    previewingEvent?.attendeeAlias && {
      label: "Attendee",
      lineContent: {
        content: previewingEvent.attendeeAlias,
        icon: sectionsIcons.user,
      },
    },
    (previewingEvent?.pickedDate || pickedDate) &&
      (previewingEvent?.duration || duration) && {
        label: "Date & Time",
        ...(pickedDate &&
          duration && {
            callbackFn: {
              label: "Edit",
              callbackFnScreen: "Available Event Days Selection",
            },
          }),
        lineContent: [
          {
            content: `${weekDays[getDay(pickedDate)]} - ${
              months[getMonth(pickedDate)]
            } ${getDate(pickedDate)}`,
            icon: sectionsIcons.calendar,
          },
          {
            content: `${getDigitalLocaleTime(
              previewingEvent?.pickedDate || pickedDate
            )} - ${getDigitalLocaleTime(
              new Date(previewingEvent?.pickedDate || pickedDate).getTime() +
                (previewingEvent?.duration || duration)
            )} ${timeZone}`,
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
  ].filter((s) => !!s)

  const isLastItem = (index: number) =>
    isNewEvent
      ? index === newEventSections.length - 1
      : organizerEvent
      ? index === organizerEventSections.length - 1
      : index === bookingEventSections.length - 1

  const renderSections = ({ item, index }: { item: SectionDetail; index: number }) => (
    <ConfirmationDetail
      key={index}
      label={item?.label}
      lineContent={item?.lineContent}
      callbackFn={item?.callbackFn}
      isLastItem={isLastItem(index)}
    />
  )
  const keyExtractor = (item: any, index: number) => `${item?.label}_${index}`

  return (
    <FlatList
      data={
        isNewEvent
          ? newEventSections
          : organizerEvent || isCalendarEventPreview
          ? organizerEventSections
          : bookingEventSections
      }
      renderItem={renderSections}
      keyExtractor={keyExtractor}
    />
  )
}
