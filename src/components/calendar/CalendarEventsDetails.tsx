import * as React from "react";

import { CalendarEventsDetail } from "./CalendarEventsDetail";

export const CalendarEventsDetails = ({ item }: any) => {
  const {
    title,
    description,
    fromTime,
    toTime,
    participants,
    setHighlightedDay,
    highlightedDay,
    listLength,
    listSection,
    organizer,
    index,
  } = item;

  return (
    <CalendarEventsDetail
      key={`${item.fromTime}_${item.toTime}`}
      title={title}
      description={description}
      fromTime={fromTime}
      toTime={toTime}
      participants={participants}
      setHighlightedDay={setHighlightedDay}
      highlightedDay={highlightedDay}
      listLength={listLength}
      listSection={listSection}
      organizer={organizer}
      index={index}
    />
  );
};
