import { Events } from "Api/Events";
import * as React from "react";

export const useEventsResults = () => {
  const [events, setEvents] = React.useState<any[] | null>(null);
  const [isLoading, setIsLoading] = React.useState<boolean>(false);

  const getEventsBySearchQuery = async (searchValue: string) => {
    try {
      const res: any = await Events.getEventsBySearch(searchValue);
      const data = res.data;

      if (data) {
        setEvents(data.result);
        setIsLoading(false);
      }
    } catch (e) {
      console.error(e);
    }
  };

  return {
    isLoading,
    setIsLoading,
    events,
    setEvents,
    getEventsBySearchQuery,
  };
};
