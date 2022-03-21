import * as R from "react";
import { Users } from "Api/Users";
import { myCalendarContext } from "contexts/contextApi";
import { ProfileContext } from "contexts/profileContext";
import { convertToCalendarEvents } from "lib/utils";
import { NewCalendarMonths } from "common/interfaces/myCalendarInterface";

export const useCalendarEvents = (id?: string) => {
  const [loadingEvents, setLoadingEvents] = R.useState<boolean>(false);
  const { setEvents, loadInitialMyCalendar, loadMyCalendar } =
    myCalendarContext();
  const { id: userId } = R.useContext(ProfileContext);

  const getEvents = (
    loadCalendar: boolean,
    currCalendarDate?: Date,
    calendarSetup?: NewCalendarMonths
  ) => {
    setLoadingEvents(true);
    fetchEvents(currCalendarDate, loadCalendar, calendarSetup);
  };

  const fetchEvents = async (
    date: any,
    withCalendarLoad?: boolean,
    setup?: NewCalendarMonths
  ) => {
    try {
      let events = await Users.getUserCalendarEvents(
        id || userId,
        date
      );
      if (events) {
        events = convertToCalendarEvents(events);

        if (events && events.length) {
          if (withCalendarLoad) {
            setEvents(events);
            if (!setup) {
              loadInitialMyCalendar();
            } else loadMyCalendar(setup);
            setLoadingEvents(false);
            return true;
          } else {
            setLoadingEvents(false);
            return events;
          }
        }
      }
    } catch (e) {
      setLoadingEvents(false);
      console.error(e);
    }
  };

  return {
    getEvents,
    loadingEvents,
    fetchEvents,
  };
};
