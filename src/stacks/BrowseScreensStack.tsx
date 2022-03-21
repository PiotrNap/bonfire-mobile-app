import * as React from "react";

import { createStackNavigator } from "@react-navigation/stack";
import { BookingContextProvider } from "contexts/bookingContext";
import {
  // AvailableDates,
  AvailableDaysSelection,
  AvailableTimes,
  DurationChoice,
  EventDescription,
} from "screens/booking/index";
import { BrowseScreen } from "screens/index";
import { WalletTopUpScreen } from "screens/onboarding";
import { DetailedConfirmation } from "screens/payments";
import { BookingStackParamList } from "common/types/navigationTypes";
import { MyCalendarProvider } from "contexts/myCalendarContext";

const Stack = createStackNavigator<BookingStackParamList>();

export const BrowseScreensStack = () => (
  <BookingContextProvider>
    <MyCalendarProvider>
      <Stack.Navigator screenOptions={{ headerShown: false }}>
        <Stack.Screen name="Browse" component={BrowseScreen} />
        <Stack.Screen name="Event Description" component={EventDescription} />
        <Stack.Screen
          name="Available Event Days Selection"
          component={AvailableDaysSelection}
        />
        {/*<Stack.Screen name="Available Dates" component={AvailableDates} />*/}
        <Stack.Screen name="Available Times" component={AvailableTimes} />
        <Stack.Screen name="Duration Choice" component={DurationChoice} />
        <Stack.Screen name="Add Funds" component={WalletTopUpScreen} />
        <Stack.Screen
          name="Booking Confirmation"
          component={DetailedConfirmation}
        />
      </Stack.Navigator>
    </MyCalendarProvider>
  </BookingContextProvider>
);
