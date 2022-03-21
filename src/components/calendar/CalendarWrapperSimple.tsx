import * as React from "react";
import { MyCalendarProvider } from "contexts/myCalendarContext";

export interface CalendarWrapperSimpleProps {
  children: React.ReactNode;
}

export const CalendarWrapperSimple = ({
  children,
}: CalendarWrapperSimpleProps) => (
  <MyCalendarProvider>{children}</MyCalendarProvider>
);
