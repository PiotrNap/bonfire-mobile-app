import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { BookingStackParamList } from "common/types/navigationTypes"
import { Sizing } from "styles/index"
import { appContext, bookingContext } from "contexts/contextApi"
import { MonthlyWrapper } from "components/calendar"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"

type Props = StackScreenProps<
  BookingStackParamList,
  "Available Event Days Selection"
>

export const AvailableDaysSelection = ({ navigation, route }: Props) => {
  const { title, image, color, titleColor } = route.params
  const { pickedDate, resetBookingState } = bookingContext()
  const { colorScheme } = appContext()
  const isDisabled = !pickedDate

  //@TODO add the organizer info to route params
  const onBackNavigationPress = () => {
    resetBookingState()
    navigation.navigate("Browse")
  }
  const onNextPress = () => navigation.navigate("Available Times", route.params)

  return (
    <EventBookingLayout
      onBackPress={onBackNavigationPress}
      screenHeader={"Select an available day"}
      eventCardColor={color}
      eventCardImage={image}
      eventCardTitle={title}
      eventCardTitleColor={titleColor}>
      <View style={styles.calendarWrapper}>
        <MonthlyWrapper isBookingCalendar={true} />
      </View>
      <View style={styles.buttonContainer}>
        <FullWidthButton
          onPressCallback={onNextPress}
          text={"Next Step"}
          colorScheme={colorScheme}
          disabled={isDisabled}
        />
      </View>
    </EventBookingLayout>
  )
}

const styles = StyleSheet.create({
  calendarWrapper: {
    flex: 1,
  },
  buttonContainer: {
    alignItems: "center",
    justifyContent: "center",
    width: "90%",
    marginVertical: Sizing.x10,
  },
})
