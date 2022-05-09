import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { EventCreationParamList } from "common/types/navigationTypes"
import { appContext } from "contexts/contextApi"
import { CalendarEventsList } from "components/calendar"
import { ErrorHandler } from "components/errors/errorHandler"
import { Calendar } from "containers/MyCalendar"
import { MyCalendarProvider } from "contexts/myCalendarContext"
import { ValueOf } from "react-native-gesture-handler/lib/typescript/typeUtils"

export interface HomeProps
  extends StackScreenProps<EventCreationParamList, "Home"> {}

export const HomeScreen = ({ navigation }: HomeProps) => {
  const { accountType, appBgColor } = appContext()

  const navigateCb = (
    name: keyof EventCreationParamList,
    params: ValueOf<EventCreationParamList>
  ) => navigation.navigate(name, params)

  return (
    <View style={{ flex: 1, backgroundColor: appBgColor }}>
      {accountType === "organizer" ? (
        <MyCalendarProvider>
          <ErrorHandler>
            <Calendar
              isRegularCalendar={true}
              isHomeScreen={true}
              navigateCb={navigateCb}
            />
          </ErrorHandler>
        </MyCalendarProvider>
      ) : (
        <View style={styles.main}>
          <CalendarEventsList navigateCb={navigateCb} isHomeScreen={true} />
        </View>
      )}
    </View>
  )
}

const styles = StyleSheet.create({
  main: {
    alignItems: "center",
    flex: 1,
  },
})
