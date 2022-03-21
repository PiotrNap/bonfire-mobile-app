import * as React from "react";
import { LayoutRectangle } from "react-native";

import { PlaceholderDay } from "./PlaceholderDay";
import { MonthlyDay } from "./days/MonthlyDay";
import { Month } from "interfaces/myCalendarInterface";
import { BookingDay } from "./days/BookingDay";
import { AvailabilityDay } from "./days/AvailabilityDay";
import { getTime } from "lib/utils";
import { monthsByName } from "common/types/calendarTypes";
import { eventCreationContext } from "contexts/contextApi";

export interface MonthProps extends Month {
  dimensions: LayoutRectangle | null;
  onPlaceholderPress: (direction: string) => void;
  isBookingCalendar?: boolean;
  isNewEventCalendar?: boolean;
  customCallback?: (arg: any | undefined) => void;
  secondCustomCallback?: (arg: Date | null) => void;
  month: string;
}

export const MonthItem = ({
  year,
  month,
  days,
  onPlaceholderPress,
  customCallback,
  isBookingCalendar = false,
  isNewEventCalendar = false,
  secondCustomCallback,
}: MonthProps) => {
  const { selectedDays, setSelectedDays } = eventCreationContext();
  const [activeDay, setActiveDay] = React.useState<number | null>(null);

  const isSelectedAvailability = React.useCallback(
    (year, month, number) => {
      return !!selectedDays?.[getTime(year, monthsByName[month], number)];
    },
    [selectedDays]
  );

  const onPressCallback = React.useCallback(
    (val: number) => {
      if (!selectedDays) {
        setSelectedDays([val]);
      } else if (selectedDays) {
        setSelectedDays([val]);
      } else {
        setSelectedDays([val]);
      }
    },
    [isNewEventCalendar]
  );

  const updateActiveDay = (num: number | null) => {
    setActiveDay(num);
    secondCustomCallback &&
      secondCustomCallback(
        num ? new Date(year, monthsByName[month], num) : null
      );
  };

  React.useEffect(() => {
    if (!isBookingCalendar && !isNewEventCalendar) {
      let isCurrMonth =
        new Date().getFullYear() === year &&
        new Date().getMonth() === monthsByName[month];
      let currDay = new Date().getDate();

      setActiveDay(isCurrMonth ? currDay : null);
    }
    customCallback && customCallback(!!selectedDays);
  }, [selectedDays, month]);

  return (
    <>
      {days.map((day) =>
        day.name === "placeholder" ? (
          <PlaceholderDay
            onPlaceholderPress={onPlaceholderPress}
            direction={day.direction != null ? day.direction : ""}
            key={`${day.name}-${day.number}`}
            number={day.number}
          />
        ) : isBookingCalendar ? (
          <BookingDay
            month={month}
            year={year}
            name={day.name}
            key={`${month}-${day.name}-${day.number}`}
            number={day.number}
            availabilities={day.availabilities}
            isAvailable={day.isAvailable}
            activeDay={activeDay}
            setActiveDay={setActiveDay}
          />
        ) : isNewEventCalendar ? (
          <AvailabilityDay
            month={month}
            year={year}
            name={day.name}
            key={`${month}-${day.name}-${day.number}`}
            number={day.number}
            isSelectedDay={isSelectedAvailability(year, month, day.number)}
            onPressCallback={onPressCallback}
          />
        ) : (
          <MonthlyDay
            name={day.name}
            key={`${month}-${day.name}-${day.number}`}
            number={day.number}
            availabilities={day.availabilities}
            activeDay={activeDay}
            updateActiveDay={updateActiveDay}
            events={day.events}
          />
        )
      )}
    </>
  );
};
