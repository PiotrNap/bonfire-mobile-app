import * as React from "react"

import { createStackNavigator } from "@react-navigation/stack"
import { BookingContextProvider } from "contexts/bookingContext"
import {
  AvailableDatesSelection,
  AvailableTimes,
  DurationChoice,
  EventDescription,
} from "screens/booking/index"
import { SearchListScreen } from "screens/index"
import { DetailedConfirmation } from "screens/payments"
import { BookingStackParamList } from "common/types/navigationTypes"
import { MyCalendarProvider } from "contexts/myCalendarContext"

const Stack = createStackNavigator<BookingStackParamList>()

export const BrowseScreensStack = () => (
  <BookingContextProvider>
    <MyCalendarProvider>
      <Stack.Navigator
        screenOptions={{ headerShown: false }}
        initialRouteName="Search List">
        <Stack.Screen name="Search List" component={SearchListScreen} />
        <Stack.Screen name="Event Description" component={EventDescription} />
        <Stack.Screen
          name="Available Event Dates Selection"
          component={AvailableDatesSelection}
        />
        <Stack.Screen name="Available Times" component={AvailableTimes} />
        <Stack.Screen name="Duration Choice" component={DurationChoice} />
        <Stack.Screen name="Booking Confirmation" component={DetailedConfirmation} />
        <Stack.Screen name="Event Details" component={DetailedConfirmation} />
      </Stack.Navigator>
    </MyCalendarProvider>
  </BookingContextProvider>
)
