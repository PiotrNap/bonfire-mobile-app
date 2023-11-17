import { Dimensions, Platform, PermissionsAndroid, Permission } from "react-native"
import { randomBytes } from "react-native-randombytes"
import {
  AvailabilitiesDay,
  Day,
  Month,
  Events,
  EventsDay,
  Availability,
  Availabilities,
} from "interfaces/myCalendarInterface"
import { months, monthsByName, weekDays } from "common/types/calendarTypes"
import { AnyObject } from "yup/lib/types"
import dayjs from "dayjs"

const IS_ANDROID = Platform.OS === "android"

export function noop() {
  return
}

export function scale(size: number, factor = 1) {
  return +((Dimensions.get("window").width / 390) * size * factor).toFixed(2)
}

export const bufferToBase64 = (val: Buffer) => {
  return !val ? val : Buffer.from(val).toString("base64")
}

export function utf8ToHex(string: string) {
  return Buffer.from(string).toString("hex")
}

export function getDeepLinkUri(path: string = ""): string {
  const scheme = "bonfire://"
  const host = "bonfire.com"
  if (IS_ANDROID) {
    let dl = scheme + host
    return path ? dl + "/" + path : dl
  } else {
    return path ? scheme + path : scheme
  }
}

export const requestAndroidPermission = async (
  permission: Permission,
  reason: string
) => {
  try {
    const isGranted = await PermissionsAndroid.request(permission, {
      title: "Bonfire app needs your permission",
      message: `We need you permission for ` + reason,
      buttonNeutral: "Ask me later",
      buttonPositive: "OK",
      buttonNegative: "Cancel",
    })

    if (IS_ANDROID && isGranted !== PermissionsAndroid.RESULTS.GRANTED) {
      return false
    }
    return true
  } catch (err) {
    return false
  }
}

export function formatDateFromMilliseconds(milliseconds: number) {
  const date = new Date(milliseconds)

  // Adding 1 to get the correct month number (1-12) since getMonth() returns 0-11
  const month = (date.getMonth() + 1).toString().padStart(2, "0") // Ensures two digits
  const day = date.getDate().toString().padStart(2, "0") // Ensures two digits
  const year = date.getFullYear()

  return `${month}-${day}-${year}`
}

/**
 * @description Returns a day with value provided as argument, otherwise it returns current day.
 * @name getDay
 * @param val (any valid value for Date js object)
 */
export function getDay(val?: any): number {
  if (val != null) return new Date(val).getDay()
  return new Date().getDay()
}

/**
 * @description Returns a date with value provided as argument, otherwise it returns current date.
 * @name getDate
 * @param val (any valid value for Date js object)
 */
export function getDate(val?: any): number {
  if (val != null) return new Date(val).getDate()
  return new Date().getDate()
}

/**
 * @description Returns a month with value provided as argument, otherwise it returns current month.
 * @name getMonth
 * @param val (any valid value for Date js object)
 */
export function getMonth(val?: any): number {
  if (val != null) return new Date(val).getMonth()
  return new Date().getMonth()
}

/**
 * @description Returns a month name with value provided as argument, otherwise it returns current month name.
 * @name getMonthName
 * @param val (any valid value for Date js object)
 */
export function getMonthName(val?: any): string {
  var month
  if (val != null) {
    month = months[new Date(val).getMonth()]
  } else {
    month = months[new Date().getMonth()]
  }
  return month
}

/**
 * @description Returns a month number with value provided as argument, otherwise it returns current month.
 * @name getMonthByName
 * @param val (any valid value for Date js object)
 */
export function getMonthByName(val?: any): number {
  var month
  if (val != null) {
    month = monthsByName[new Date(val).getMonth()]
  } else {
    month = monthsByName[new Date().getMonth()]
  }
  return month
}

export function getYear(val?: any): number {
  if (val != null) return new Date(val).getFullYear()
  return new Date().getFullYear()
}

export function getTime(year?: number, month?: number, day?: number): number {
  if (day != null && month != null && year != null)
    return new Date(year, month, day).getTime()
  if (month != null && year != null) return new Date(year, month).getTime()
  return new Date().getTime()
}

/**
 * @param month - month of current previewing calendar
 * @param year - year of current previewing calendar
 */
export function isSixMonthsLater(year: number, month: number) {
  const futureDate = new Date().setMonth(new Date().getMonth() + 6)
  const _year = new Date(futureDate).getFullYear()
  const _month = new Date(futureDate).getMonth()

  return new Date(_year, _month).getTime() <= new Date(year, month).getTime()
}

/**
 * @param month - month of current previewing calendar
 * @param year - year of current previewing calendar
 */
export function isSixMonthsBefore(year: number, month: number) {
  const pastDate = new Date().setMonth(new Date().getMonth() - 6)
  const _year = new Date(pastDate).getFullYear()
  const _month = new Date(pastDate).getMonth()

  return new Date(_year, _month).getTime() >= new Date(year, month).getTime()
}

/**
 * @description Check whether two given timestamps are the same date.
 * @param val1, val2
 */
export function areEqualDates(val1: number, val2: number): boolean {
  const sameYear = getYear(val1) === getYear(val2)
  const sameMonth = getMonth(val1) === getMonth(val2)
  const sameDay = getDate(val1) === getDate(val2)

  return sameYear && sameMonth && sameDay
}

/**
 * @description Returns false if the given date is a past date (starting from 1 day before)
 * @param year
 * @param month
 * @param day
 */
export const isPastDate = (year: number, month: string, day: number) => {
  return dayjs(new Date(year, monthsByName[month], day)).isBefore(dayjs(), "day")
}

/**
 * @description Checks whether two dates are at the same day in time
 * @param date1
 * @param date2
 */
export const isSameDay = (date1: number | string, date2: number | string) => {
  const sameYear = dayjs(date1).year() === dayjs(date2).year()
  const sameMonth = dayjs(date1).month() === dayjs(date2).month()
  const sameDay = dayjs(date1).day() === dayjs(date2).day()

  return sameYear && sameMonth && sameDay
}

export const availableDaysLeftInCurrMonth = (availabilities: number[]): boolean =>
  !!availabilities.find(
    (date) =>
      dayjs(date).month() === dayjs().month() && dayjs(date).date() > dayjs().date()
  )

/**
 *   @description This will return an array with next/previous month/s with
 *    number of total days, name of the first day (eg. 'Monday'), and
 *    names of the months.
 *
 *    Next months are starting from current month (or passed as fromMonth),
 *    while previous months will omit current month.
 */
export function getCalendarMonth(
  nextMonths = false,
  previousMonths = false,
  fromMonth?: number,
  fromYear?: number,
  availabilities: any[] | undefined | null = [],
  scheduledEvents: Events[] | undefined | null = [],
  startFromCustomMonth: boolean = false
): Month[] {
  var month = fromMonth != null ? fromMonth : new Date().getMonth()
  var year = fromYear != null ? fromYear : new Date().getFullYear()
  var currMonthIndex = fromMonth != null && !startFromCustomMonth ? month + 1 : month

  // if current month is December and fromYear isn't specified, meaning
  // we are at the last month of the current year
  if (previousMonths && month === 11 && fromYear === undefined) {
    year++
    // set the first month to January (is index 0)
    currMonthIndex = 0
  }

  // ...and if the month is January and fromYear isn't specified, meaning
  // we are at first month of the current year
  if (!nextMonths && month === 0 && fromYear === undefined) {
    year--

    // set the first month to December (is index 11)
    currMonthIndex = 11
  }

  // get the first day of the month: eg. 1 (Monday)
  var firstDayIndex = new Date(year, month).getDay()

  var monthsWithDays: Month[] = []
  var currYear = year
  var currDayIndex = firstDayIndex
  var numOfDays = 0
  var scheduledYear =
    scheduledEvents &&
    scheduledEvents.find((schedEvts: any) => schedEvts.year === currYear)
  var availableYear =
    availabilities && availabilities.find((avail: any) => avail.year === currYear)

  if (nextMonths) {
    if (month === 11) {
      currMonthIndex = 0
      currYear++
    }

    for (let i = currMonthIndex; month === 11 ? i < 1 : i <= month + 1; i++) {
      let days: Day[] = []
      let events: EventsDay[] = []
      let availableSlots: AvailabilitiesDay[] = []

      if (availableYear != null) {
        var availableDays = availableYear.months.find(
          (month: any) => month.month === months[i]
        )

        availableDays?.days.map((availDay: AvailabilitiesDay) =>
          availableSlots.push(availDay)
        )
      }

      if (scheduledYear != null) {
        var scheduledDays = scheduledYear.months.find(
          (month: any) => month.month === months[i]
        )
        scheduledDays?.days.map((schedDay: EventsDay) => events.push(schedDay))
      }

      const firstDay = new Date(currYear, currMonthIndex).getDay()
      const firstDayName = weekDays[firstDay]

      // We want to start iterating on months days starting at the first
      // day of the current month
      currDayIndex = firstDay

      for (let j = 1; isValidDate(j, currMonthIndex, currYear); j++) {
        let availableDay = availableSlots.find((s) => s.day === j)

        let dayEvents = events.find((e) => e.day === j)?.events
        let isPastDate = new Date(currYear, currMonthIndex, j) < new Date()

        let day: Day = {
          name: weekDays[currDayIndex],
          number: j,
        }

        if (isLastWeek(j, firstDay)) {
          day.isLastWeek = true
        }
        if (availableDay != null && !isPastDate) {
          day.isAvailable = true
        }
        if (dayEvents != null) {
          day.events = [...dayEvents]
        }

        /* Check the day of the week, if it's Sunday -
         * set the next one to Monday
         */
        if (currDayIndex === 6) {
          currDayIndex = 0
        } else {
          currDayIndex++
        }
        numOfDays++
        days.push(day)
      }
      days = insertFirstWeekPlaceholders(firstDay, days, currMonthIndex, currYear)
      days = insertLastWeekPlaceholders(firstDay, days, currMonthIndex, currYear)

      monthsWithDays.push({
        name: months[currMonthIndex],
        numOfEvents: events ? events.length : 0,
        numOfAvailabilities: availableSlots ? availableSlots.length : 0,
        firstDayName,
        numOfDays,
        year: currYear,
        days,
      })

      currMonthIndex++
      numOfDays = 0
      if (currMonthIndex > 11) {
        currMonthIndex = 0
        currYear += 1
      }
    }
  } else if (previousMonths) {
    // Previous months need to omit the current, so we substract these values:
    --month
    --currMonthIndex

    // If we have passed a fromMonth value but haven't specified fromYear:
    if (fromMonth != null) --currMonthIndex

    // if the fromMonth is January, start iterating from month December
    if (fromMonth === 0) {
      month = 11
      currMonthIndex = 11

      // If we need to go a year back
      if (fromYear != null) --currYear
    }

    if (fromMonth === 11 && !fromYear) {
      currYear = getYear()
      currMonthIndex = 10
    }

    for (let i = currMonthIndex - 1; i < month; i++) {
      const firstDay = new Date(currYear, currMonthIndex).getDay()
      const firstDayName = weekDays[firstDay]
      let days: Day[] = []
      let events: EventsDay[] = []
      let availableSlots: AvailabilitiesDay[] = []
      if (availableYear != null) {
        var availableDays = availableYear.months.find(
          (month: any) => month.month === months[i + 1]
        )
        availableDays?.days.map((availDay: AvailabilitiesDay) =>
          availableSlots.push(availDay)
        )
      }

      if (scheduledYear != null) {
        var scheduledDays = scheduledYear.months.find(
          (month: any) => month.month === months[i + 1]
        )
        scheduledDays?.days.map((schedDay: EventsDay) => events.push(schedDay))
      }

      // We want to start iterating on months days starting at the first
      // day of the current month
      currDayIndex = firstDay

      for (let j = 1; isValidDate(j, currMonthIndex, currYear); j++) {
        let availableDay = availableSlots.find((s) => s.day === j)
        let dayEvents = events.find((e) => e.day === j)?.events
        let isPastDate = new Date(currYear, currMonthIndex, j) < new Date()

        let day: Day = {
          name: weekDays[currDayIndex],
          number: j,
        }

        if (isLastWeek(j, firstDay)) {
          day.isLastWeek = true
        }
        if (availableDay != null && !isPastDate) {
          day.isAvailable = true
        }
        if (dayEvents != null) {
          day.events = [...dayEvents]
        }

        /* Check the day of the week, if it's Sunday -
         * set the next one to Monday
         */
        if (currDayIndex === 6) {
          currDayIndex = 0
        } else {
          currDayIndex++
        }
        numOfDays++
        days.push(day)
      }

      days = insertFirstWeekPlaceholders(firstDay, days, currMonthIndex, year)
      days = insertLastWeekPlaceholders(firstDay, days, currMonthIndex, currYear)

      monthsWithDays.push({
        name: months[currMonthIndex],
        firstDayName,
        numOfEvents: events ? events.length : 0,
        numOfAvailabilities: availableSlots ? availableSlots.length : 0,
        numOfDays,
        year: currYear,
        days,
      })
      currMonthIndex--
      numOfDays = 0
      if (currMonthIndex < 0) {
        currMonthIndex = 11
        currYear -= 1
      }
    }
    monthsWithDays.reverse()
    return monthsWithDays
  }
  return monthsWithDays
}

export function isValidDate(day: number, month: number, year: number): boolean {
  return new Date(year, month, day).getMonth() === month ? true : false
}

/**
 * @description Check whether the first week should have placeholder
 * displayed on a week
 */
export function insertLastWeekPlaceholders(
  firstDay: number,
  days: Day[],
  month: number,
  year: number
): Day[] {
  var placeholdersDays: Day[] = []

  // Calculate how many days remain untill the end of full calendar view (42 days)
  const totalNumOfPlaceholders =
    42 - firstDay - new Date(year, month + 1, -1).getDate() - 1

  for (let i = totalNumOfPlaceholders; i > 0; i--) {
    placeholdersDays.unshift({
      name: "placeholder",
      number: i,
      direction: "next",
    })
  }
  return [...days, ...placeholdersDays]
}

/**
 * @description Check whether the first week should have placeholder
 * displayed on a week
 */
export function insertFirstWeekPlaceholders(
  firstDay: number,
  days: Day[],
  month: number,
  year: number
): Day[] {
  var placeholdersDays: Day[] = []

  // total number of days at previous month
  var numOfDays = new Date(year, month, 0).getDate()

  for (let i = 0; i < firstDay; i++) {
    placeholdersDays.unshift({
      name: "placeholder",
      number: numOfDays,
      direction: "previous",
    })
    numOfDays--
  }
  return [...placeholdersDays, ...days]
}

/**
 * @description Check whether the day is of the last week (for layout
 *  purposes)
 */
export function isLastWeek(currDay: number, firstDay: number): boolean {
  switch (firstDay) {
    case 1:
      return currDay >= 28 ? true : false
    case 2:
      return currDay >= 27 ? true : false
    case 3:
      return currDay >= 26 ? true : false
    case 4:
      return currDay >= 25 ? true : false
    case 5:
      return currDay >= 31 ? true : false
    case 6:
      return currDay >= 30 ? true : false
    case 0:
      return currDay >= 29 ? true : false
    default:
      return false
  }
}

/**
 *  @description Get the time in clock-like format
 *
 *  @property time - time in milliseconds or date string
 *  @property offset - clock offset, either '12' or '24' (default '24')
 */
export function getDigitalTime(time: number | string | Date, offset = "24"): string {
  var hours = new Date(time).getHours()
  var minutes = new Date(time).getMinutes()
  var abbreviation
  if (offset === "12") {
    abbreviation = hours >= 12 && hours <= 24 ? "pm" : "am"
    if (hours > 12 && hours <= 23) {
      hours -= 12
    } else if (hours === 0) {
      hours = 12
    }
  }

  return `${hours}:${minutes <= 9 ? "0" + minutes : minutes} ${abbreviation ?? ""}`
}

/**
 *  @description returns a string of local time based on 'time' and 'locale'
 */
export function getDigitalLocaleTime(
  time: number | Date,
  locale: string = "en"
): string | void {
  var timeString: any = new Date(time).toLocaleTimeString(locale)
  var abbreviation: string = ""

  timeString = timeString.split(" ")
  if (timeString == null || timeString.length === 0) return

  abbreviation = timeString[1]
  timeString.pop()
  timeString = timeString[0].split(":")
  timeString.pop()
  timeString = timeString.join(":")

  return timeString + " " + abbreviation
}

/**
 *  @description returns a string based on length of time passed as arg
 */
export function getTimeSpanLength(time: number): string {
  // 'getHours' returns 1 even if we pass 1 min as a time to 'Date' object
  let hours = new Date(time).getHours() - 1
  let minutes = new Date(time).getMinutes()

  if (!hours && minutes) {
    return `${minutes} mins`
  } else if (hours === 1 && !minutes) {
    return `${hours} hour`
  } else if (hours > 1 && !minutes) {
    return `${hours} hours`
  } else {
    return `${hours}${minutes && "." + minutes} hrs`
  }
}

/**
 * @description Returns date used to show on event card component
 *
 * @param fromDate: number
 * @param toDate: number
 *
 * @returns date: string
 */
export function getEventCardDate(
  fromDate: number | string | Date,
  toDate: number | string | Date
) {
  let dateString = ""
  const fromMonthDay = getDate(fromDate)
  const toMonthDay = getDate(toDate)

  // if the events happens in the same month
  if (getMonthName(fromDate) === getMonthName(toDate)) {
    if (fromMonthDay === toMonthDay)
      return `${fromMonthDay} ${getMonthName(fromDate).slice(0, 3)}`
    dateString +=
      fromMonthDay + "-" + toMonthDay + " " + getMonthName(fromDate).slice(0, 3)

    return dateString
  }

  // if it's first day - only attach the month name (first 3 letters)
  if (getDate(fromDate) === 1) {
    dateString += getMonthName(fromDate).slice(0, 3)
  } else {
    dateString += getMonthName(fromDate).slice(0, 3) + " " + getDate(fromDate)
  }

  // if it's first day - only attach the month name (first 3 letters)
  if (getDate(toDate) === 1) {
    dateString += " - " + getMonthName(toDate).slice(0, 3)
  } else {
    dateString += " - " + getMonthName(toDate).slice(0, 3) + " " + getDate(toDate)
  }

  return dateString
}

/**
 * @description Get locale timezone as a string, e.g 'UTC +2'
 */
export function getLocaleTimezone(): string {
  const offset = new Date().getTimezoneOffset()

  return `UTC ${offset < 0 && "+"}${-offset / 60}`
}

export function getDevicesTimeZone(): string {
  return Intl.DateTimeFormat().resolvedOptions().timeZone
}

/**
 * Uses randomBytes method from react-native-randombytes to
 * generate array of random bytes
 *
 * @param bytes - number of bytes
 */
export function getRandomKey(bytes: number): string {
  return randomBytes(bytes).join("")
}

export const roundDateMinutes = (date: Date): Date => {
  return new Date(date.setMinutes(Math.round(date.getMinutes() / 10) * 10))
}

export const sortEventAvailabilities = (
  availabilities: Availability[],
  sortType: "asc" | "desc"
) =>
  availabilities.sort((a, b) => {
    if (
      dayjs(a.to) > dayjs(b.to) ||
      (dayjs(a.to) === dayjs(b.to) && a.maxDuration > b.maxDuration)
    )
      return sortType === "asc" ? 1 : -1
    return sortType === "asc" ? -1 : 1
  })

/**
 * @description Helper function to convert the event-selected-days into calendar ready availabilities
 */
export const convertToCalendarAvailabilities = (
  selectedDays: {
    [index: string]: number
  },
  availableDayTimeSlots: any[]
): Availabilities[] => {
  const timesInMill: number[] = Object.values(selectedDays)
  const sortedAvailableSlots = sortEventAvailabilities(availableDayTimeSlots, "asc")
  const lastAvailableDayTimeSlot = sortedAvailableSlots[sortedAvailableSlots.length - 1]
  // TODO this will eventually be allowed to change by organizers when creating a new event
  const availableUntil =
    new Date(lastAvailableDayTimeSlot.to).getTime() -
    lastAvailableDayTimeSlot.maxDuration * 60000

  let calendarAvailabilities: Availabilities[] = []
  let currentYear, currentMonth, currentDay

  for (const time of timesInMill) {
    const year = new Date(time).getFullYear()
    const month = months[new Date(time).getMonth()]
    const day = new Date(time).getDate()

    // Case when it's the first time or the year has changed
    if (year !== currentYear && month !== currentMonth && day !== currentDay) {
      currentYear = year
      currentMonth = month
      currentDay = day

      calendarAvailabilities.push({
        year,
        months: [{ month, days: [{ day, availableUntil }] }],
      })
      continue
    }

    // Case when year is the same but month and day has changed
    if (month !== currentMonth && currentDay !== day) {
      currentMonth = month
      currentDay = day

      const yearToPushIndex = calendarAvailabilities.findIndex((val) => val.year === year)

      calendarAvailabilities[yearToPushIndex].months.push({
        month,
        days: [{ day, availableUntil }],
      })
      continue
    }

    // Case when year and month are the same but day has changed
    if (currentDay !== day) {
      currentDay = day

      const yearToPushIndex = calendarAvailabilities.findIndex((val) => val.year === year)
      const monthToPushIndex = calendarAvailabilities[yearToPushIndex].months.findIndex(
        (val: any) => val.month === month
      )

      calendarAvailabilities[yearToPushIndex].months[monthToPushIndex].days.push({
        day,
        availableUntil,
      })
      continue
    }
  }

  return calendarAvailabilities
}

export const convertToCalendarEvents = (organizerEvents: { [index: string]: any[] }) => {
  var bookedSlots = organizerEvents?.bookedSlots
  var scheduledSlots = organizerEvents?.scheduledSlots
  var activeEvents = organizerEvents?.events

  var calendarEvents: any[] = []
  var currentYear: number, currentMonth: string, currentDay: number

  const iterateOverEvents = (
    arr: any[] = [],
    type: "booked slot" | "scheduled slot" | "active slot"
  ) => {
    if (!arr.length) return
    var eventsInEachMonth: AnyObject = {}

    // Because we want to show each available day for an ongoing
    // event of our organizers, we have to create separate slots
    // for each of the day in each event hosted by that person
    if (type === "active slot") {
      let newArr: any[] = []
      for (const event of arr) {
        for (const day of Object.values(event.selectedDays)) {
          newArr.push(Object.assign({ availableAt: day }, event))

          // this will let us know how many days are in current month
          if (!eventsInEachMonth[getYear(day)]?.[getMonthName(day)]) {
            eventsInEachMonth[getYear(day)] = {
              [getMonthName(day)]: {
                events: [
                  {
                    id: event.id,
                    days: Object.values(event.selectedDays)
                      .filter((sd: any) => getMonthName(sd) === getMonthName(day))
                      .sort(),
                  },
                ],
              },
            }
          } else if (
            !eventsInEachMonth[getYear(day)]?.[getMonthName(day)]?.events.find(
              (e: any) => e.id === event.id
            )
          ) {
            eventsInEachMonth[getYear(day)][getMonthName(day)].events.push({
              id: event.id,
              days: Object.values(event.selectedDays)
                .filter((sd: any) => getMonthName(sd) === getMonthName(day))
                .sort(),
            })
          }
        }
      }

      if (newArr.length) arr = newArr
    }

    var currEventId: string = ""
    var sortedAvailabilites: Availability[] = []

    for (const val of arr) {
      const fromDate = type === "active slot" ? val.availableAt : val.bookedDate
      const year = new Date(fromDate).getFullYear()
      const month = months[new Date(fromDate).getMonth()]
      const day = new Date(fromDate).getDate()

      const yearIndex = calendarEvents.findIndex((obj) => obj.year === year)

      let monthIndex = -1,
        dayIndex = -1

      if (yearIndex > -1) {
        monthIndex = calendarEvents[yearIndex].months.findIndex(
          (obj: any) => obj.month === month
        )
      }

      if (yearIndex > -1 && monthIndex > -1) {
        dayIndex = calendarEvents[yearIndex].months[monthIndex].days.findIndex(
          (obj: any) => obj.day === day
        )
      }
      // evaluating 'calendarEvents[yearIndex].months[monthIndex].days')
      // {"days": [{"day": 27, "events": [Array]}], "month": "October"}

      const slotObject: AnyObject = {
        id: val.id,
        organizerId: val.organizerId,
        organizerAlias: val.organizerAlias,
        attendeeId: val?.attendeeId,
        attendeeAlias: val?.attendeeAlias,
        eventTitle: val.eventTitle || val.title,
        eventDescription: val.eventDescription || val.description,
        bookedDuration: val?.bookedDuration,
        bookedDate: val?.bookedDate,
        txHash: val?.txHash,
        fromDate: val?.fromDate,
        toDate: val?.toDate,
        hourlyRate: val?.hourlyRate,
        type,
      }

      if (type === "active slot") {
        if (currEventId != val.id) {
          sortedAvailabilites = sortEventAvailabilities(val.availabilities, "desc")
          currEventId = val.id
        }
        // we need to know what's the max available day in a month
        // for a particular event
        slotObject.toTimeSlot = sortedAvailabilites[0].to
        slotObject.fromTimeSlot = sortedAvailabilites[sortedAvailabilites.length - 1].from
        slotObject.availabilities = val.availabilities
        slotObject.availableAt = val.availableAt

        // return max day for active event in a given month
        const eventsInMonth = eventsInEachMonth[year]?.[month]?.events.find(
          (e: AnyObject) => e.id === val.id
        )

        if (eventsInMonth && eventsInMonth.days?.length) {
          slotObject.maxAvailableMonthDate =
            eventsInMonth.days[eventsInMonth.days.length - 1]
          slotObject.minAvailableMonthDate = eventsInMonth.days[0]
        }
      }

      // Case when it's the first time or year has changed
      if (year !== currentYear && month !== currentMonth && day !== currentDay) {
        currentYear = year
        currentMonth = month
        currentDay = day

        // if year doesn't exists yet
        if (yearIndex < 0 && monthIndex < 0 && dayIndex < 0) {
          calendarEvents.push({
            year,
            months: [
              {
                month,
                days: [
                  {
                    day,
                    events: [slotObject],
                  },
                ],
              },
            ],
          })
          // if month doesnt' exists yet
        } else if (monthIndex < 0 && dayIndex < 0) {
          calendarEvents[yearIndex].months.push({
            month,
            days: [
              {
                day,
                events: [slotObject],
              },
            ],
          })
          // if day doesn't exists yet
        } else if (dayIndex < 0) {
          calendarEvents[yearIndex].months[monthIndex].days.push({
            day,
            events: [slotObject],
          })
        }

        continue
      }

      // Case when year is the same but month and day has changed
      if (month !== currentMonth && currentDay !== day) {
        currentMonth = month
        currentDay = day

        if (monthIndex < 0 && dayIndex < 0) {
          calendarEvents[yearIndex].months.push({
            month,
            days: [
              {
                day,
                events: [slotObject],
              },
            ],
          })
        } else if (dayIndex < 0) {
          calendarEvents[yearIndex].months[monthIndex].days.push({
            day,
            events: [slotObject],
          })
        }

        continue
      }

      // Case when year and month are the same but day has changed
      if (currentDay !== day) {
        currentDay = day

        calendarEvents[yearIndex].months[monthIndex].days.push({
          day,
          events: [slotObject],
        })

        continue
      }

      // if it's the same day
      calendarEvents[yearIndex].months[monthIndex].days[dayIndex].events.push(slotObject)
    }
  }

  iterateOverEvents(bookedSlots, "booked slot")
  iterateOverEvents(scheduledSlots, "scheduled slot")
  iterateOverEvents(activeEvents, "active slot")

  return calendarEvents
}
