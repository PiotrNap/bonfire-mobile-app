import * as React from "react"

import { createStackNavigator } from "@react-navigation/stack"
import { MyEventsStackParamList } from "common/types/navigationTypes"
import { EventDescription } from "screens/booking"
import { MyEvents } from "screens/organizer/MyEvents"
import {
  AvailableDaysSelection,
  AvailableTimeSelection,
  ImageCoverSelection,
  NewEventDescription,
} from "screens/organizer/newEvent"
import { DetailedConfirmation } from "screens/payments"
import { EventCardCustomization } from "screens/organizer/newEvent/EventCardCustomization"
import { EventCreationContextProvider } from "contexts/eventCreationContext"

const Stack = createStackNavigator<MyEventsStackParamList>()

export const MyEventsStack = () => {
  return (
    <EventCreationContextProvider>
      <Stack.Navigator initialRouteName="My Events" headerMode="none">
        <Stack.Screen name="My Events" component={MyEvents} />
        <Stack.Screen
          name="New Event Description"
          component={NewEventDescription}
        />
        <Stack.Screen
          name="Available Days Selection"
          component={AvailableDaysSelection}
        />
        <Stack.Screen
          name="Available Time Selection"
          component={AvailableTimeSelection}
        />
        <Stack.Screen
          name="Image Cover Selection"
          component={ImageCoverSelection}
        />
        <Stack.Screen
          name="Event Card Customization"
          component={EventCardCustomization}
        />
        <Stack.Screen
          name="Event Confirmation Details"
          component={DetailedConfirmation}
        />
        <Stack.Screen name="Event Description" component={EventDescription} />
        <Stack.Screen name="Event Details" component={DetailedConfirmation} />
      </Stack.Navigator>
    </EventCreationContextProvider>
  )
}
