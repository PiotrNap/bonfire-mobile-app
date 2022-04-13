import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { OrganizerTabParamList } from "common/types/navigationTypes"
import { appContext } from "contexts/contextApi"
import { CalendarEventsList } from "components/calendar"
import { ErrorHandler } from "components/errors/errorHandler"
import { Calendar } from "containers/MyCalendar"
import { MyCalendarProvider } from "contexts/myCalendarContext"

export interface HomeProps
  extends StackScreenProps<OrganizerTabParamList, "Home"> {}

export const HomeScreen = ({}: HomeProps) => {
  const { accountType, appBgColor } = appContext()

  return (
    <View style={{ flex: 1, backgroundColor: appBgColor }}>
      {accountType === "organizer" ? (
        <MyCalendarProvider>
          <ErrorHandler>
            <Calendar isRegularCalendar={true} isHomeScreen={true} />
          </ErrorHandler>
        </MyCalendarProvider>
      ) : (
        <View style={styles.main}>
          <CalendarEventsList isHomeScreen={true} />
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
