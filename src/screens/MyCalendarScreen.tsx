import * as React from "react"
import { Calendar } from "containers/MyCalendar"
import { ErrorHandler } from "components/errors/errorHandler"
import { EventCreationParamList } from "common/types/navigationTypes"
import { ValueOf } from "react-native-gesture-handler/lib/typescript/typeUtils"

export const MyCalendarScreen = ({ navigation }: any) => {
  const navigateCb = (
    name: keyof EventCreationParamList,
    params: ValueOf<EventCreationParamList>
  ) => navigation.navigate(name, params)
  return (
    <ErrorHandler>
      <Calendar navigateCb={navigateCb} />
    </ErrorHandler>
  )
}
