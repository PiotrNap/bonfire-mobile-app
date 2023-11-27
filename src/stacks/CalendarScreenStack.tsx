import * as React from "react"

import { createStackNavigator } from "@react-navigation/stack"
import { EventCreationParamList } from "common/types/navigationTypes"
import { MyCalendarScreen } from "screens/index"

const Stack = createStackNavigator<EventCreationParamList>()

export const CalendarScreenStack = ({ route }: any) => {
  return (
    <Stack.Navigator screenOptions={{ headerShown: false }}>
      <Stack.Screen
        name="Home"
        component={MyCalendarScreen}
        initialParams={route.params}
      />
    </Stack.Navigator>
  )
}
