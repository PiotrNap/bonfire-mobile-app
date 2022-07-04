import { Availability } from "common/interfaces/myCalendarInterface"
import dayjs, { Dayjs } from "dayjs"
import { sortEventAvailabilities } from "lib/utils"
import * as React from "react"

export const useDurationSlots = (
  minTimeSlotDuration: number,
  maxTimeSlotDuration: number | undefined,
  availabilities: Availability[],
  pickedDate: number | Dayjs
) => {
  const [timeSlots, setTimeSlots] = React.useState<number[] | null>(null)

  React.useEffect(() => {
    if (minTimeSlotDuration != null && maxTimeSlotDuration != null) {
      if (minTimeSlotDuration === maxTimeSlotDuration)
        return setTimeSlots([minTimeSlotDuration * 60 * 1000])

      // create an array of accumulated time blocks based on maxTimeSlotDuration
      let newTimeSlots: number[] = []
      let totalTimeDuration = minTimeSlotDuration
      let maxAvailableTime = dayjs(
        sortEventAvailabilities(availabilities, "desc")[0].to
      )
      pickedDate = dayjs(pickedDate)
      let maxToPickDate = pickedDate
        .set("hours", maxAvailableTime.hour())
        .set("minutes", maxAvailableTime.minute())
        .set("seconds", 0)

      while (
        totalTimeDuration < maxTimeSlotDuration ||
        (totalTimeDuration === maxTimeSlotDuration &&
          pickedDate.add(totalTimeDuration, "minutes") <= maxToPickDate)
      ) {
        newTimeSlots.push(totalTimeDuration * 60 * 1000)
        totalTimeDuration += minTimeSlotDuration
      }

      setTimeSlots(newTimeSlots)
    }
  }, [maxTimeSlotDuration, minTimeSlotDuration, pickedDate])

  return { timeSlots, setTimeSlots }
}
