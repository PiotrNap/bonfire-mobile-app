import { Platform, Share } from "react-native"

import { ANDROID_API_URL, IOS_API_URL } from "@env"
import { signChallenge } from "./tweetnacl"
import Aes from "react-native-aes-crypto"
import Crypto from "crypto"
import base64 from "base64-js"

import { monthsByName } from "common/types/calendarTypes"
import { Auth } from "../services/Api/Auth"
import { getDeepLinkUri, isPastDate } from "./utils"
import { createNestedPath } from "./navigation"
import { DEEP_LINKING_URLS } from "common/types/navigationTypes"
import Toast from "react-native-toast-message"
import { CalendarUtils, TimelineEventProps } from "react-native-calendars"
import { MarkedDates } from "react-native-calendars/src/types"
import { EventTimeWindow } from "common/interfaces/myCalendarInterface"
import { EventAvailability } from "common/interfaces/newEventInterface"
import { Colors } from "styles/index"

const { applyOpacity } = Colors

const DEFAULT_ERROR_MSG = "Something went wrong. Please reload the app and try again."

const SCHEDULED_COLOR = "#1E40AF"
const BOOKED_COLOR = "#FECACA"
const EVENT_COLOR = "#DBEAFE"

export const isAndroid = Platform.OS === "android"
export const isIOS = Platform.OS === "ios"

export function chunkArray(array: any[], chunkSize: number) {
  return array.reduce((acc, item, index) => {
    const chunkIndex = Math.floor(index / chunkSize)

    if (!acc[chunkIndex]) {
      acc[chunkIndex] = [] // start a new chunk
    }

    acc[chunkIndex].push(item)

    return acc
  }, [])
}

export function findEarliestAndLatestDates(eventAvailabilities: EventAvailability[]) {
  if (eventAvailabilities.length === 0) {
    return { earliestDate: "", latestDate: "" }
  }

  let earliestDate = new Date(eventAvailabilities[0].from)
  let latestDate = new Date(eventAvailabilities[0].to)

  eventAvailabilities.forEach((event) => {
    const fromDate = new Date(event.from)
    const toDate = new Date(event.to)

    if (fromDate < earliestDate) {
      earliestDate = fromDate
    }
    if (toDate > latestDate) {
      latestDate = toDate
    }
  })

  return {
    earliestDate: earliestDate.toISOString(),
    latestDate: latestDate.toISOString(),
  }
}

// Constructs YYYY-MM-DD format from '/' separated date
export const formatDateWithDashes = (date: Date) => {
  return [
    date.getFullYear(),
    (date.getMonth() + 1).toString().padStart(2, "0"),
    date.getDate().toString().padStart(2, "0"),
  ].join("-")
}

// returns a date string in a format required by the calendar library
const today = new Date()
export const getCalendarDate = (offset = 0) =>
  CalendarUtils.getCalendarDateString(new Date().setDate(today.getDate() + offset))

export function convertToEventAvailabilityUTC(
  dates: MarkedDates,
  timeSlots: EventTimeWindow[]
): EventAvailability[] {
  const eventAvailabilities: EventAvailability[] = []

  Object.keys(dates).forEach((date) => {
    timeSlots.forEach((slot) => {
      // Parse local time
      const fromTimeLocal = new Date(`${date} ${slot.from.hour}:${slot.from.minutes}`)
      const toTimeLocal = new Date(`${date} ${slot.to.hour}:${slot.to.minutes}`)

      // Convert to UTC
      const fromTimeUTC = fromTimeLocal.toISOString()
      const toTimeUTC = toTimeLocal.toISOString()

      const eventSlot: EventAvailability = {
        from: fromTimeUTC,
        to: toTimeUTC,
        isFullyBooked: false,
        maxDuration: slot.maxDuration,
        minDuration: slot.minDuration,
        slots: [],
      }

      const numOfSlots =
        (toTimeLocal.getTime() - fromTimeLocal.getTime()) / (slot.minDuration * 60 * 1000)

      for (let i = 0; i < numOfSlots; i++) {
        eventSlot.slots.push({
          isAvailable: true,
          bookingId: "",
          from:
            i === 0
              ? fromTimeUTC
              : new Date(
                  fromTimeLocal.setMinutes(fromTimeLocal.getMinutes() + slot.minDuration)
                ).toISOString(),
        })
      }
      eventAvailabilities.push(eventSlot)
    })
  })

  return eventAvailabilities
}

export function convertFromEventAvailability(
  eventAvailabilities: EventAvailability[],
  isBookingCalendar: boolean
): { dates: MarkedDates; timeWindows: EventTimeWindow[] } {
  const localDates = {}
  const localTimeWindows: EventTimeWindow[] = []

  eventAvailabilities.forEach((eventAvailability) => {
    // Convert UTC to local time
    const toTimeLocal = new Date(eventAvailability.to)
    const fromTimeLocal = new Date(eventAvailability.from)

    // Extract local dates using the system's locale
    const fromDate = formatDateWithDashes(fromTimeLocal)
    const toDate = formatDateWithDashes(toTimeLocal)

    // Add dates to localDates with handling for date changes
    ;[fromDate, toDate].forEach((date, idx) => {
      const splitDate = date.split("-")
      const year = Number(splitDate[0])
      const month = Number(splitDate[1]) - 1
      const day = Number(splitDate[2])

      // @TODO test it with an old event
      if (isPastDate(year, month, day)) return

      if (!localDates[date]) {
        if (isBookingCalendar) {
          localDates[date] = {
            customStyles: {
              container: {
                backgroundColor: applyOpacity(Colors.blueCalendarCard.s400, 0.5), // New background color for selected date
              },
              text: {
                color: Colors.primary.neutral, // Color of the text for selected state
              },
            },
            selected: false,
            utcDate: new Date(idx === 0 ? eventAvailability.from : eventAvailability.to)
              .toISOString()
              .split("T")[0],
          }
        } else {
          localDates[date] = {
            selected: false,
            utcDate: new Date(idx === 0 ? eventAvailability.from : eventAvailability.to)
              .toISOString()
              .split("T")[0],
          }
        }
      }
    })

    // Format time slots
    const timeSlot = {
      fromDate,
      toDate,
      slots: eventAvailability.slots,
      from: { hour: fromTimeLocal.getHours(), minutes: fromTimeLocal.getMinutes() },
      to: { hour: toTimeLocal.getHours(), minutes: toTimeLocal.getMinutes() },
      fromUTC: {
        hour: fromTimeLocal.getUTCHours(),
        minutes: fromTimeLocal.getUTCMinutes(),
      },
      toUTC: { hour: toTimeLocal.getUTCHours(), minutes: toTimeLocal.getUTCMinutes() },
      maxDuration: eventAvailability.maxDuration,
      minDuration: eventAvailability.minDuration,
    }

    // Add time slot to the array if it's not already included
    localTimeWindows.push(timeSlot)
  })

  return { dates: localDates, timeWindows: localTimeWindows }
}

export function getMarkedDatesFromUserCalendarData(data) {
  const markedDates = {}

  const convertToLocalDate = (utcDate) => {
    const date = new Date(utcDate)
    return date.toISOString().split("T")[0] // Convert to local date and format
  }

  data.bookedSlots.forEach((slot) => {
    const dateKey = convertToLocalDate(slot.fromDate)
    if (markedDates[dateKey]?.dots.some((md) => md.key === "booking-slot")) return

    if (!markedDates[dateKey]) {
      markedDates[dateKey] = { dots: [] }
    }

    markedDates[dateKey].dots.push({
      key: "booking-slot",
      color: BOOKED_COLOR,
      selectedDotColor: BOOKED_COLOR,
    })
  })

  data.events.forEach((event) => {
    const dateKey = convertToLocalDate(event.fromDate)
    if (markedDates[dateKey]?.dots.some((md) => md.key === "event")) return

    if (!markedDates[dateKey]) {
      markedDates[dateKey] = { dots: [] }
    }

    markedDates[dateKey].dots.push({
      key: "event",
      color: EVENT_COLOR,
      selectedDotColor: EVENT_COLOR,
    })
  })

  data.scheduledSlots.forEach((slot) => {
    const dateKey = convertToLocalDate(slot.fromDate)
    if (markedDates[dateKey]?.dots.some((md) => md.key === "scheduled-slot")) return

    if (!markedDates[dateKey]) {
      markedDates[dateKey] = { dots: [] }
    }

    markedDates[dateKey].dots.push({
      key: "scheduled-slot",
      color: SCHEDULED_COLOR,
      selectedDotColor: SCHEDULED_COLOR,
    })
  })

  return markedDates
}

export function createTimelineEvents(data) {
  const timelineArray: TimelineEventProps[] = []
  const groupedTimeline = {}

  const parseDateTime = (dateTime, duration) => {
    const startDate = new Date(dateTime)
    const endDate = new Date(startDate.getTime() + duration)
    return {
      start: startDate.toISOString().replace("Z", ""),
      end: endDate.toISOString().replace("Z", ""),
    }
  }

  data.bookedSlots.forEach((item) => {
    const { start, end } = parseDateTime(item.fromDate, item.duration)
    const dateKey = start.split("T")[0] // Extract the date part for grouping

    if (!groupedTimeline[dateKey]) {
      groupedTimeline[dateKey] = []
    }

    groupedTimeline[dateKey].push({
      id: item.id,
      start: start,
      end: end,
      title: item.eventTitle || "Event",
      color: BOOKED_COLOR,
    })
  })

  data.scheduledSlots.forEach((item) => {
    const { start, end } = parseDateTime(item.fromDate, item.duration)
    const dateKey = start.split("T")[0] // Extract the date part for grouping

    if (!groupedTimeline[dateKey]) {
      groupedTimeline[dateKey] = []
    }

    groupedTimeline[dateKey].push({
      id: item.id,
      start: start,
      end: end,
      title: item.eventTitle || "Event",
      color: SCHEDULED_COLOR,
    })
  })

  data.events.forEach((event) => {
    const dateKey = event.fromDate.split("T")[0] // Extract the date part for grouping
    if (!groupedTimeline[dateKey]) {
      groupedTimeline[dateKey] = []
    }

    groupedTimeline[dateKey].push({
      id: event.id,
      start: new Date(event.fromDate).toISOString().replace("Z", ""),
      end: new Date(event.toDate).toISOString().replace("Z", ""),
      title: event.title || "Event",
      color: EVENT_COLOR, // Assign color based on type
    })
  })

  return { groupedTimeline, timelineArray }
}

export function generateTimeSlotsForDateInMilliseconds(
  localDatesAndSlots: any,
  selectedDate: string
) {
  const [localDates, localTimeSlots] = localDatesAndSlots
  const timeSlots = []

  if (!localDates[selectedDate]) {
    // If the selected date is not available
    return timeSlots
  }

  localTimeSlots.forEach((slot) => {
    // Convert times to Date objects
    const slotStartTime = new Date(
      `${selectedDate}T${slot.from.hour.toString().padStart(2, "0")}:${slot.from.minutes
        .toString()
        .padStart(2, "0")}`
    )
    const slotEndTime = new Date(
      `${selectedDate}T${slot.to.hour.toString().padStart(2, "0")}:${slot.to.minutes
        .toString()
        .padStart(2, "0")}`
    )

    for (
      let time = new Date(slotStartTime);
      time < slotEndTime;
      time.setMinutes(time.getMinutes() + slot.minDuration)
    ) {
      // Ensure the slot doesn't exceed the end time
      if (new Date(time.getTime() + slot.minDuration * 60000) > slotEndTime) {
        break
      }
      timeSlots.push(time.getTime()) // Get UTC milliseconds
    }
  })

  return timeSlots
}

/**
 *  Takes index of the selected day in the weeek
 *  and returns recurring days of the same index in current month.
 *
 *  For example:
 *   - Sunday (1-08-2021)
 *   - Sunday (8-08-2021)
 */
export const getRecurringMonthDays = (index: number, year: number, month: string) => {
  const daysArray: number[] = []

  const numOfDays = new Date(year, monthsByName[month] + 1, 0).getDate()
  const firstDayOfWeek = new Date(year, monthsByName[month]).getDay()

  var firstDayToSelect =
    firstDayOfWeek > index ? 7 - firstDayOfWeek + index + 1 : 1 + (index - firstDayOfWeek)
  // Calculate number of weeks (+1 because starting from current selected day)
  var numOfWeeks = Math.floor((numOfDays - firstDayToSelect) / 7 + 1)

  for (; numOfWeeks > 0; numOfWeeks--) {
    daysArray.push(new Date(year, monthsByName[month], firstDayToSelect).getTime())
    firstDayToSelect = firstDayToSelect + 7
  }

  return daysArray
}

/**
 * Starts challenge sequence to obtain JWT from the server.
 * Returns {id, username, accessToken, expiresIn , ???} or  `null` if failed.
 */
export const startChallengeSequence = async (
  privKey: string,
  deviceID: string,
  id: string // user-ID
): Promise<{ [index: string]: string } | void> => {
  let res = await Auth.requestChallenge({
    deviceID,
    id,
  })
  let { challenge } = res

  if (challenge) {
    let signature: any = await signChallenge(
      base64.toByteArray(challenge),
      base64.toByteArray(privKey)
    )
    if (signature) {
      let res = await Auth.requestAccessToken({
        challenge,
        signature,
        id,
        deviceID,
      })
      if (res) return res
    }
  }
  privKey = ""
}

export const getApiUrl = (url: string): string => {
  if (url[0] !== "/") url = "/" + url
  const baseUrl = Platform.OS === "ios" ? IOS_API_URL : ANDROID_API_URL

  return baseUrl + url
}

export const getFormDataFromFilePath = (filePath: string) => {
  const fileChunks = filePath.split(".")
  const fileType = fileChunks[fileChunks.length - 1]
  const formData = new FormData()

  formData.append("file", {
    uri: Platform.OS === "ios" ? filePath.replace("file://", "") : filePath,
    name: `photo.${fileType}`,
    type: `image/${fileType}`,
  })

  return formData
}

export const shareEvent = async (id: string) => {
  try {
    const result = await Share.share({
      message:
        getDeepLinkUri(
          createNestedPath([
            DEEP_LINKING_URLS.NAVIGATION,
            DEEP_LINKING_URLS.NAVIGATION,
            DEEP_LINKING_URLS.BROWSE,
          ])
        ) + `/${id}`,
    })
    if (result.action === Share.sharedAction) {
      if (result.activityType) {
        // shared with activity type of result.activityType
      } else {
        // shared
      }
    } else if (result.action === Share.dismissedAction) {
    }
  } catch (e) {}
}

export const isUUID = (val: string): boolean => /((\w{4,12}-?)){5}/.test(val)

export const encryptWithPassword = async (
  value: string,
  password: string
): Promise<undefined | string> => {
  const salt = Crypto.randomBytes(16)
  const nonce = Crypto.randomBytes(12)
  const base64Value = Buffer.from(value).toString("base64")
  const key = await Aes.pbkdf2(password, salt.toString("hex"), 5000, 256, "sha512")

  const encrypted = await Aes.encrypt(
    base64Value,
    key,
    nonce.toString("hex"),
    "aes-256-ctr"
  )

  // Concatenating salt, nonce, and encrypted data
  return salt.toString("hex") + nonce.toString("hex") + encrypted
}

export const decryptWithPassword = async (
  cipherText: string,
  password: string
): Promise<undefined | string> => {
  // Extracting salt, nonce, and encrypted data from cipherText
  const salt = Buffer.from(cipherText.slice(0, 32), "hex")
  const nonce = Buffer.from(cipherText.slice(32, 56), "hex")
  const encrypted = cipherText.slice(56)

  const key = await Aes.pbkdf2(password, salt.toString("hex"), 5000, 256, "sha512")

  const decryptedBase64 = await Aes.decrypt(
    encrypted,
    key,
    nonce.toString("hex"),
    "aes-256-ctr"
  )

  return Buffer.from(decryptedBase64, "base64").toString()
}

export function showSuccessToast(header: string, body: string): void {
  Toast.show({ text1: header, text2: body })
}
export function showInfoToast(header: string, body: string): void {
  Toast.show({ type: "info", text1: header, text2: body })
}

export function showErrorToast(e?: any, header?: string): void {
  const isDev = typeof __DEV__ === "boolean" && __DEV__
  if (isDev) console.error("From Toast: ", e)

  const body = typeof e === "string" ? e : e?.message || e?.msg || DEFAULT_ERROR_MSG

  //@TODO send possible erorr to Sentry
  Toast.show({
    type: "error",
    text1: header || "Error",
    text2: body,
  })
}
