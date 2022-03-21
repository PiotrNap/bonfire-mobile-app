import * as React from "react";

export const useDurationSlots = (
  minTimeSlotDuration: number,
  maxTimeSlotDuration: number | undefined
) => {
  const [timeSlots, setTimeSlots] = React.useState<number[] | null>(null);

  React.useEffect(() => {
    if (minTimeSlotDuration != null && maxTimeSlotDuration != null) {
      // create an array of accumulated time blocks based on maxTimeSlotDuration
      let newTimeSlots: number[] = [];
      let totalTimeDuration = minTimeSlotDuration;

      while (totalTimeDuration < maxTimeSlotDuration) {
        newTimeSlots.push(totalTimeDuration * 60 * 1000);
        totalTimeDuration += minTimeSlotDuration;
      }

      setTimeSlots(newTimeSlots);
    }
  }, [maxTimeSlotDuration, minTimeSlotDuration]);

  return { timeSlots, setTimeSlots };
};
