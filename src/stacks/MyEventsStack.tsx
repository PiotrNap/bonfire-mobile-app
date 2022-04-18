import * as React from "react"

import { createStackNavigator } from "@react-navigation/stack"
import { MyEventsStackParamList } from "common/types/navigationTypes"
import { EventDescription } from "screens/booking"
import { MyEvents } from "screens/organizer/MyEvents"
import { DetailedConfirmation } from "screens/payments"

const Stack = createStackNavigator<MyEventsStackParamList>()

export const MyEventsStack = () => {
  return (
    <Stack.Navigator initialRouteName="My Events" headerMode="none">
      <Stack.Screen name="My Events" component={MyEvents} />
      <Stack.Screen name="Event Description" component={EventDescription} />
      <Stack.Screen name="Event Details" component={DetailedConfirmation} />
    </Stack.Navigator>
  )
}
