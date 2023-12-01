import { EventAvailability } from "common/interfaces/newEventInterface"
import * as React from "react"

export const useAvailabilities = (
  availabilities: EventAvailability[],
  selectedDate: string
) => {
  const [currAvailabilities, setCurrentAvailabilities] = React.useState<
    undefined | number[]
  >(undefined)

  React.useEffect(() => {
    // Calculate how many time slots should we render depending on
    // organizer time block. Eg. organizerTimeBlock 30min = 8:00, 8:30, 9:00...
    let currentDateAvailabilities = availabilities.sort(
      (a: any, b: any) => a.from - b.from
    )
    currentDateAvailabilities.filter((availability) =>
      availability.from.includes(selectedDate)
    )

    const currTimeSlots: number[] = []

    currentDateAvailabilities.forEach((availability: any) => {
      const to = new Date(availability.to).getTime()
      let from = new Date(availability.from).getTime()

      while (from < to) {
        // prevent presenting the same time slots (like 19:40, 19:40)
        if (!currTimeSlots.includes(from)) currTimeSlots.push(from)
        from = from + availability.minDuration * 60 * 1000 // millisedonds
      }
    })

    setCurrentAvailabilities(currTimeSlots.sort())
  }, [availabilities])

  return { currAvailabilities, setCurrentAvailabilities }
}
