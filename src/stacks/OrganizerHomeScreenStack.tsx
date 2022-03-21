import * as React from "react";

import { createStackNavigator } from "@react-navigation/stack";
import { EventCreationContextProvider } from "contexts/eventCreationContext";
import { HomeScreen } from "screens/HomeScreen";
import {
  AvailableDaysSelection,
  AvailableTimeSelection,
  ImageCoverSelection,
  NewEventDescription,
} from "screens/organizer/newEvent";
import { EventCreationParamList } from "common/types/navigationTypes";
import { EventCardCustomization } from "screens/organizer/newEvent/EventCardCustomization";
import { DetailedConfirmation } from "screens/payments";

const Stack = createStackNavigator<EventCreationParamList>();

export const OrganizerHomeScreenStack = ({ route }: any) => {
  return (
    <EventCreationContextProvider>
      <Stack.Navigator screenOptions={{ headerShown: false }}>
        <Stack.Screen
          name="Home"
          component={HomeScreen}
          initialParams={route.params}
        />
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
      </Stack.Navigator>
    </EventCreationContextProvider>
  );
};
