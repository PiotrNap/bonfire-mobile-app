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
  formatDateFromMilliseconds,
  getDate,
  getDay,
  getDigitalLocaleTime,
  getDigitalTime,
  getMonth,
  getTimeSpanLength,
  isSameDay,
} from "lib/utils"
import { AnyObject } from "yup/lib/types"
import { ProfileContext } from "contexts/profileContext"
import { hexToUtf8, lovelaceToAda } from "lib/wallet/utils"

interface ConfirmationDetails {
  isNewEvent: boolean
  isCalendarEventPreview: boolean
  organizerEvent?: AnyObject
  bookedEvent?: AnyObject
  withFlatList?: boolean
}

export const ConfirmationDetails = ({
  isNewEvent = false,
  isCalendarEventPreview,
  organizerEvent,
  bookedEvent,
  withFlatList = true,
}: any) => {
  const { colorScheme } = appContext()
  const { timeZone } = React.useContext(ProfileContext)
  var {
    duration,
    durationCost,
    pickedStartTime,
    previewingEvent: bookingPreviewingEvent,
  } = bookingContext()
  const {
    textContent,
    hourlyRate,
    imageURI,
    eventCardColor,
    eventTitleColor,
    selectedDates,
    fromDate,
    toDate,
  } = eventCreationContext()
  const isLightMode = colorScheme === "light"
  const previewingEvent = bookingPreviewingEvent || bookedEvent

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

  /**
   * Displayed from users' main calendar
   */
  const organizerEventSections: any[] = [
    organizerEvent?.title && {
      label: "Title",
      lineContent: {
        content: organizerEvent?.title,
      },
    },
    organizerEvent?.description && {
      label: "Description",
      lineContent: {
        content: organizerEvent?.description,
      },
    },
    organizerEvent?.fromDate && {
      label: "Date",
      lineContent: !isSameDay(organizerEvent?.fromDate, organizerEvent?.toDate)
        ? [
            {
              content: `Start: ${weekDays[getDay(organizerEvent?.fromDate)]} - ${
                months[getMonth(organizerEvent?.fromDate)]
              } ${getDate(organizerEvent?.fromDate)}`,
            },
            {
              content: `End: ${weekDays[getDay(organizerEvent?.toDate)]} - ${
                months[getMonth(organizerEvent?.toDate)]
              } ${getDate(organizerEvent?.toDate)}`,
            },
          ]
        : [
            {
              content: `${new Date(organizerEvent?.fromDate).toLocaleString()}`,
            },
          ],
    },
    organizerEvent?.availableAt && {
      label: "Date",
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

  /**
   * Displayed during event creation confirmation
   */
  const newEventSections: any[] = [
    textContent?.title && {
      label: "Title",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "New Event Description",
      },
      lineContent: {
        content: textContent.title,
      },
    },
    textContent?.summary && {
      label: "Description",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "New Event Description",
      },
      lineContent: {
        content: textContent.summary,
      },
    },
    selectedDates &&
      fromDate &&
      toDate && {
        label: "Date",
        callbackFn: {
          label: "Edit",
          callbackFnScreen: "Available Days Selection",
        },
        lineContent: !isSameDay(fromDate, toDate)
          ? [
              {
                content: `Start: ${formatDateFromMilliseconds(fromDate)}`,
              },
              {
                content: `End: ${formatDateFromMilliseconds(toDate)}`,
              },
              {
                content: `Time Zone: ${timeZone}`,
              },
            ]
          : [
              {
                content: `${formatDateFromMilliseconds(fromDate)}`,
              },
              {
                content: `Time Zone: ${timeZone}`,
              },
            ],
      },
    {
      label: "Hourly Rate",
      lineContent: hourlyRate.map((hr) =>
        hr.displayName === "ada"
          ? {
              content: hr.count,
              icon: hr.displayName === "ada" ? <AdaIcon {...iconStyles} /> : null,
            }
          : {
              content: `${hr.displayName} - ${Number(hr.count)}`,
            }
      ),
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

  /**
   * Displayed during event booking confirmation
   */
  const bookingEventSections: SectionDetail[] = [
    previewingEvent?.title && {
      label: "Event Title",
      lineContent: {
        content: previewingEvent.title,
      },
    },
    previewingEvent?.organizerAlias && {
      label: "Organizer",
      lineContent: {
        content: previewingEvent.organizerAlias,
      },
    },
    previewingEvent?.attendeeAlias && {
      label: "Attendee",
      lineContent: {
        content: previewingEvent.attendeeAlias,
      },
    },
    pickedStartTime &&
      duration && {
        label: "Date",
        callbackFn: {
          label: "Edit",
          callbackFnScreen: "Available Event Dates Selection",
          param: { event: bookedEvent },
        },
        lineContent: {
          content: new Date(pickedStartTime).toLocaleString(),
        },
      },
    duration && {
      label: "Duration",
      callbackFn: {
        label: "Edit",
        callbackFnScreen: "Duration Choice",
        param: { event: bookedEvent },
      },
      lineContent: {
        content: `${duration / (1000 * 60 * 60)}hr`,
      },
    },
    durationCost && {
      label: "Total cost",
      lineContent: Array.from(durationCost).map((costEntries) =>
        costEntries[0] === "lovelace"
          ? {
              content: `${lovelaceToAda(BigInt(costEntries[1]))}`,
              icon: sectionsIcons.ada,
            }
          : {
              content: `(${hexToUtf8(costEntries[1].name)}) ${costEntries[1].count}`,
            }
      ),
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
  const data = isNewEvent
    ? newEventSections
    : organizerEvent || isCalendarEventPreview
    ? organizerEventSections
    : bookingEventSections

  return withFlatList ? (
    <FlatList data={data} renderItem={renderSections} keyExtractor={keyExtractor} />
  ) : (
    <>{data.map((item, index) => renderSections({ item, index }))}</>
  )
}
