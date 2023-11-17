import * as React from "react"
import { MyCalendarProvider } from "contexts/myCalendarContext"

export interface CalendarWrapperProps {
  children: React.ReactNode
}

export const CalendarWrapper = ({ children }: CalendarWrapperProps) => (
  <MyCalendarProvider>{children}</MyCalendarProvider>
)
