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

export const SCHEDULED_COLOR_LIGHT = "#FFCCE6" // pink-ish
export const SCHEDULED_COLOR_DARK = "#FF99C2"
export const BOOKED_COLOR_LIGHT = "#FFE6CC" // yellow-ish
export const BOOKED_COLOR_DARK = "#FFD9B3"
export const EVENT_COLOR_LIGHT = "#CCFFFF" // blue-ish
export const EVENT_COLOR_DARK = "#99CCCC"

export const isAndroid = Platform.OS === "android"
export const isIOS = Platform.OS === "ios"

export function AreEqualObjects(fstObj: any, sndObj: any): boolean {
  if (typeof fstObj !== "object" || typeof sndObj !== "object")
    throw new Error("One of parameters is not type of object")

  let equal = false
  for (let k of Object.keys(fstObj)) {
    equal = fstObj[k] === sndObj[k]
  }

  return equal
}

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

export function formatLocalDate(utcDate) {
  const date = new Date(utcDate)
  const options = {
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
    timeZoneName: "short",
  }

  return date.toLocaleString("en-US", options)
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
    const isFullyBooked = eventAvailability.slots.every((slot) => !slot.isAvailable)
    if (isFullyBooked) return

    // Convert UTC to local time
    const toTimeLocal = new Date(eventAvailability.to)
    const fromTimeLocal = new Date(eventAvailability.from)

    const fromDate = formatDateWithDashes(fromTimeLocal)
    const toDate = formatDateWithDashes(toTimeLocal)

    if (new Date() > toTimeLocal) return
    ;[fromDate, toDate].forEach((date, idx) => {
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
    date.toLocaleDateString
    return date.toISOString().split("T")[0] // Convert to local date and format
  }

  data.bookedSlots.forEach((slot) => {
    const startDateKey = CalendarUtils.getCalendarDateString(new Date(slot.fromDate))
    const endDateKey = CalendarUtils.getCalendarDateString(
      new Date(new Date(slot.fromDate).getTime() + slot.duration)
    )
    if (markedDates[startDateKey]?.dots.some((md) => md.key === "booking-slot")) return

    if (!markedDates[startDateKey]) {
      markedDates[startDateKey] = { dots: [] }
    }

    if (startDateKey !== endDateKey) {
      if (!markedDates[endDateKey]) {
        markedDates[endDateKey] = { dots: [] }
      }
      if (markedDates[endDateKey]?.dots.some((md) => md.key === "booking-slot")) return

      markedDates[startDateKey].dots.push({
        key: "booking-slot",
        color: BOOKED_COLOR_DARK,
        selectedDotColor: BOOKED_COLOR_DARK,
      })
      markedDates[endDateKey].dots.push({
        key: "booking-slot",
        color: BOOKED_COLOR_DARK,
        selectedDotColor: BOOKED_COLOR_DARK,
      })
    } else
      markedDates[startDateKey].dots.push({
        key: "booking-slot",
        color: BOOKED_COLOR_DARK,
        selectedDotColor: BOOKED_COLOR_DARK,
      })
  })

  data.events.forEach((event) => {
    return event.availabilities.forEach((availability) => {
      const startDateKey = CalendarUtils.getCalendarDateString(
        new Date(availability.from)
      )
      const endDateKey = CalendarUtils.getCalendarDateString(new Date(availability.to))

      if (markedDates[startDateKey]?.dots.some((md) => md.key === "event")) return

      if (!markedDates[startDateKey]) {
        markedDates[startDateKey] = { dots: [] }
      }

      if (startDateKey !== endDateKey) {
        if (!markedDates[endDateKey]) {
          markedDates[endDateKey] = { dots: [] }
        }
        if (markedDates[endDateKey]?.dots.some((md) => md.key === "event")) return

        markedDates[startDateKey].dots.push({
          key: "event",
          color: EVENT_COLOR_DARK,
          selectedDotColor: EVENT_COLOR_DARK,
        })
        markedDates[endDateKey].dots.push({
          key: "event",
          color: EVENT_COLOR_DARK,
          selectedDotColor: EVENT_COLOR_DARK,
        })
      } else
        markedDates[startDateKey].dots.push({
          key: "event",
          color: EVENT_COLOR_DARK,
          selectedDotColor: EVENT_COLOR_DARK,
        })
    })
  })

  data.scheduledSlots.forEach((slot) => {
    const startDateKey = CalendarUtils.getCalendarDateString(new Date(slot.fromDate))
    const endDateKey = CalendarUtils.getCalendarDateString(
      new Date(new Date(slot.fromDate).getTime() + slot.duration)
    )
    if (markedDates[startDateKey]?.dots.some((md) => md.key === "scheduled-slot")) return

    if (!markedDates[startDateKey]) {
      markedDates[startDateKey] = { dots: [] }
    }

    if (startDateKey !== endDateKey) {
      if (!markedDates[endDateKey]) {
        markedDates[endDateKey] = { dots: [] }
      }
      if (markedDates[endDateKey]?.dots.some((md) => md.key === "scheduled-slot")) return

      markedDates[startDateKey].dots.push({
        key: "scheduled-slot",
        color: SCHEDULED_COLOR_DARK,
        selectedDotColor: SCHEDULED_COLOR_DARK,
      })
      markedDates[endDateKey].dots.push({
        key: "scheduled-slot",
        color: SCHEDULED_COLOR_DARK,
        selectedDotColor: SCHEDULED_COLOR_DARK,
      })
    } else
      markedDates[startDateKey].dots.push({
        key: "scheduled-slot",
        color: SCHEDULED_COLOR_DARK,
        selectedDotColor: SCHEDULED_COLOR_DARK,
      })
  })

  return markedDates
}

export function createTimelineEvents(data) {
  const timelineArray: TimelineEventProps[] = []
  const timelineEvents = {}

  data.bookedSlots.forEach((item) => {
    const start = new Date(item.fromDate).toTimeString().split(" ")[0]
    const end = new Date(new Date(item.fromDate).getTime() + item.duration)
      .toTimeString()
      .split(" ")[0]
    const startDateKey = CalendarUtils.getCalendarDateString(new Date(item.fromDate))
    const endDateKey = CalendarUtils.getCalendarDateString(
      new Date(new Date(item.fromDate).getTime() + item.duration)
    )

    if (!timelineEvents[startDateKey]) {
      timelineEvents[startDateKey] = []
    }
    if (startDateKey !== endDateKey) {
      if (!timelineEvents[endDateKey]) {
        timelineEvents[endDateKey] = []
      }
      timelineEvents[startDateKey].push({
        id: item.id,
        start: `${startDateKey} ${start}`,
        end: `${endDateKey} ${end}`,
        title: item.eventTitle || "Event",
        summary: `w/ ${item.organizerAlias}`,
        color: BOOKED_COLOR_LIGHT,
      })
      timelineEvents[endDateKey].push({
        id: item.id,
        start: `${startDateKey} ${start}`,
        end: `${endDateKey} ${end}`,
        title: item.eventTitle || "Event",
        summary: `w/ ${item.organizerAlias}`,
        color: BOOKED_COLOR_LIGHT,
      })
    } else
      timelineEvents[startDateKey].push({
        id: item.id,
        start: `${startDateKey} ${start}`,
        end: `${endDateKey} ${end}`,
        title: item.eventTitle || "Event",
        summary: `w/ ${item.organizerAlias}`,
        color: BOOKED_COLOR_LIGHT,
      })
  })

  data.scheduledSlots.forEach((item) => {
    const start = new Date(item.fromDate).toTimeString().split(" ")[0]
    const end = new Date(new Date(item.fromDate).getTime() + item.duration)
      .toTimeString()
      .split(" ")[0]
    const startDateKey = CalendarUtils.getCalendarDateString(new Date(item.fromDate))
    const endDateKey = CalendarUtils.getCalendarDateString(
      new Date(new Date(item.fromDate).getTime() + item.duration)
    )

    if (!timelineEvents[startDateKey]) {
      timelineEvents[startDateKey] = []
    }
    // if event end-date stretches to one day after due to time-zone
    // ... end-date can't end up 1 day before ...
    if (startDateKey !== endDateKey) {
      if (!timelineEvents[endDateKey]) {
        timelineEvents[endDateKey] = []
      }
      timelineEvents[startDateKey].push({
        id: item.id,
        start: `${startDateKey} ${start}`,
        end: `${endDateKey} ${end}`,
        title: item.eventTitle || "Event",
        summary: `w/ ${item.organizerAlias}`,
        color: SCHEDULED_COLOR_LIGHT,
      })
      timelineEvents[endDateKey].push({
        id: item.id,
        start: `${startDateKey} ${start}`,
        end: `${endDateKey} ${end}`,
        title: item.eventTitle || "Event",
        summary: `w/ ${item.organizerAlias}`,
        color: SCHEDULED_COLOR_LIGHT,
      })
    } else
      timelineEvents[startDateKey].push({
        id: item.id,
        start: `${startDateKey} ${start}`,
        end: `${endDateKey} ${end}`,
        title: item.eventTitle || "Event",
        summary: `w/ ${item.organizerAlias}`,
        color: SCHEDULED_COLOR_LIGHT,
      })
  })

  data.events.forEach((event) => {
    return event.availabilities.forEach((availability) => {
      const startDateKey = CalendarUtils.getCalendarDateString(
        new Date(availability.from)
      )
      const endDateKey = CalendarUtils.getCalendarDateString(new Date(availability.to))
      const end = new Date(availability.to).toTimeString().split(" ")[0]
      const start = new Date(availability.from).toTimeString().split(" ")[0]

      if (!timelineEvents[startDateKey]) {
        timelineEvents[startDateKey] = []
      }

      // if event end-date stretches to one day after due to time-zone
      // ... end-date can't end up 1 day before ...
      if (startDateKey !== endDateKey) {
        if (!timelineEvents[endDateKey]) {
          timelineEvents[endDateKey] = []
        }

        timelineEvents[startDateKey].push({
          id: event.id,
          start: `${startDateKey} ${start}`,
          end: `${endDateKey} ${end}`,
          title: event.title || "Event",
          color: EVENT_COLOR_LIGHT,
        })

        timelineEvents[endDateKey].push({
          id: event.id,
          start: `${startDateKey} ${start}`,
          end: `${endDateKey} ${end}`,
          title: event.title || "Event",
          color: EVENT_COLOR_LIGHT,
        })
      } else
        timelineEvents[startDateKey].push({
          id: event.id,
          start: `${startDateKey} ${start}`,
          end: `${endDateKey} ${end}`,
          title: event.title || "Event",
          color: EVENT_COLOR_LIGHT,
        })
    })
  })

  return { timelineEvents, timelineArray }
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
export function showInfoToast(body: string, header: string): void {
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
