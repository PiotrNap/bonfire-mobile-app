import { Events } from "Api/Events";
import * as React from "react";

export const useEventsPagination = () => {
  const [events, setEvents] = React.useState<any[]>([]);
  const [eventsPage, setEventsPage] = React.useState<number>(1);
  const [eventsLimit, setEventsLimit] = React.useState<number>(10);
  const [isLoading, setIsLoading] = React.useState<boolean>(true);

  React.useEffect(() => {
    (async () => {
      await getEventsPaginated();
      setIsLoading(false);
    })();
  }, []);

  const getEventsPaginated = async (page?: number, isRefreshing = false) => {
    try {
      const res = await Events.getAllEvents({
        limit: eventsLimit,
        page: page ?? eventsPage,
      });

      if (res) {
        setEvents((prev) => [...(!isRefreshing ? prev : []), ...res.result]);
        setEventsPage(page ?? 1);
        setIsLoading(false);
      }
    } catch (e) {
      console.error(e);
    }
  };

  return {
    isLoading,
    events,
    eventsPage,
    setEventsPage,
    eventsLimit,
    setEventsLimit,
    getEventsPaginated,
  };
};
