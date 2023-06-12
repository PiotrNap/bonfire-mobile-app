import * as React from "react"
import { View, StyleSheet, } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import {
  BookingStackParamList,
  DEEP_LINKING_URLS,
} from "common/types/navigationTypes"
import { Sizing } from "styles/index"
import { appContext, bookingContext } from "contexts/contextApi"
import { MonthlyWrapper } from "components/calendar"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"
import { Checkbox } from "components/forms/Checkbox"
import { useGoogleAuth } from "lib/hooks/useGoogleAuth"
import { createNestedPath } from "lib/navigation"

type Props = StackScreenProps<
  BookingStackParamList,
  "Available Event Days Selection"
>

export const AvailableDaysSelection = ({ navigation, route }: Props) => {
  const { title, image, color, titleColor } = route.params
  const { pickedDate, resetBookingState, setCreateGoogleCalEvent } =
    bookingContext()
  const { colorScheme, validGoogleOAuth } = appContext()
  const [acceptedCheckbox, setAcceptedChecbox] = React.useState<boolean>(false)
  const [error, setError] = React.useState<any>({ isVisible: false, type: "" })
  const isDisabled = !pickedDate

  const googleCallback = () => {
    setCreateGoogleCalEvent(acceptedCheckbox)
    navigation.navigate("Available Times", route.params)
  }
  const { isRequesting, isInitialRequesting, startGoogleAuthentication } =
    useGoogleAuth(googleCallback, setError)

  const onNextPress = async () => {
    if (error.isVisible) setError({ isVisible: false, type: "" })

    if (acceptedCheckbox && !validGoogleOAuth) {
      try {
        await startGoogleAuthentication(
          createNestedPath([
            DEEP_LINKING_URLS.NAVIGATION,
            DEEP_LINKING_URLS.BROWSE,
            DEEP_LINKING_URLS.AVAILABLE_TIMES,
          ])
        )
      } catch (e) {
        console.error("New error ", e)
        setError({ isVisible: true, type: "GoogleOauth" })
      }
    } else {
      acceptedCheckbox && setCreateGoogleCalEvent(acceptedCheckbox)
      navigation.navigate("Available Times", route.params)
    }
  }
  const onBackNavigationPress = () => {
    resetBookingState()
    navigation.navigate("Browse")
  }
  const onCheckBoxPress = () => {
    setError({ isVisible: false, type: "" })
    setAcceptedChecbox((prev) => !prev)
  }

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
      <View style={styles.messageWrapper}>
        <Checkbox
          onCheckBoxPress={onCheckBoxPress}
          acceptedCheckbox={acceptedCheckbox}>
          Schedule this event on my Google calendar.
        </Checkbox>
      </View>
      <View style={styles.buttonContainer}>
        <FullWidthButton
          onPressCallback={onNextPress}
          text={"Next Step"}
          colorScheme={colorScheme}
          disabled={isDisabled}
          loadingIndicator={isRequesting}
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
    marginBottom: Sizing.x10,
    marginTop: Sizing.x5,
  },
  messageWrapper: {
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    marginTop: Sizing.x5,
  },
})
