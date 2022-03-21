import * as React from "react";

import { getDate, getMonthName, getYear } from "lib/utils";

export const useScheduledTimes = (
  scheduledEvents: any,
  pickedDate: number | null,
  timeBlock: number
) => {
  const [scheduledTimes, setScheduledTimes] = React.useState<
    undefined | number[]
  >(undefined);

  React.useEffect(() => {
    if (scheduledEvents != null && pickedDate != null) {
      let currTimeSlots: number[] = [];

      let currSchedEvents = scheduledEvents
        .find((obj: any) => obj.year === getYear(pickedDate))
        ?.months.find((obj: any) => obj.month === getMonthName(pickedDate))
        ?.days.find((obj: any) => obj.day === getDate(pickedDate))
        ?.scheduledEvents.forEach((evt: any) => {
          // this means that scheduled event is longer than time block
          if (evt.toTime - evt.fromTime > timeBlock * 60 * 1000) {
            let startingTime = evt.fromTime;

            while (startingTime < evt.toTime) {
              currTimeSlots.push(startingTime);
              // add separate scheduled events per 1 time block
              startingTime = startingTime + timeBlock * 60 * 1000; // millisedonds
            }
          } else {
            currTimeSlots.push(evt.fromTime);
          }
        });

      setScheduledTimes(currTimeSlots);
    }
  }, [scheduledEvents]);

  return { scheduledTimes, setScheduledTimes };
};
