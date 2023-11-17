import * as React from "react"
import { View, StyleSheet, Pressable, ScrollView, Text } from "react-native"

import { LeftArrowIcon } from "assets/icons"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { SafeAreaView } from "react-native-safe-area-context"
import { Colors, Sizing } from "styles/index"
import { HeaderText } from "components/rnWrappers/headerText"
import { StackScreenProps } from "@react-navigation/stack"
import { DEEP_LINKING_URLS, EventCreationParamList } from "common/types/navigationTypes"
import { MonthlyWrapper } from "components/calendar"
import { CalendarWrapper } from "components/calendar/"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useGoogleAuth } from "lib/hooks/useGoogleAuth"
import { createNestedPath } from "lib/navigation"
import { HintBox } from "components/modals/HintBox"
import { getFromEncryptedStorage, setToEncryptedStorage } from "lib/encryptedStorage"
import { UserSettings } from "common/interfaces/appInterface"
import { Checkbox } from "components/forms/Checkbox"
import { BodyText } from "components/rnWrappers/bodyText"
import { showErrorToast } from "lib/helpers"

type Props = StackScreenProps<EventCreationParamList, "Available Days Selection">

export const AvailableDaysSelection = (props: Props) => {
  const navigation = props.navigation
  const { colorScheme, validGoogleOAuth } = appContext()
  const {
    selectedDays,
    setDateFrame,
    removeSelectedDays,
    removeSelectedWeeks,
    setGCalEventsBooking,
    eventType,
  } = eventCreationContext()
  const [acceptedCheckbox, setAcceptedChecbox] = React.useState<boolean>(false)
  const [hintBoxVisible, setHintBoxVisible] = React.useState<boolean>(false)
  const [error, setError] = React.useState<any>({ isVisible: false, type: "" })

  const googleOauthCallback = () => {
    setGCalEventsBooking(acceptedCheckbox)
    navigation.navigate("Available Time Selection")
  }
  const { isRequesting, isInitialRequesting, startGoogleAuthentication } = useGoogleAuth(
    googleOauthCallback,
    setError
  )

  React.useEffect(() => {
    ;(async () => {
      const userSettings: UserSettings = await getFromEncryptedStorage("user-settings")
      if (!userSettings?.eventCreationHintHidden) setHintBoxVisible(true)
    })()
  }, [])

  React.useEffect(() => {
    if (error.isVisible) showErrorToast(null, error.type)
  }, [error])

  const isLightMode = colorScheme === "light"
  const isDisabledBtn = selectedDays === null || !Object.entries(selectedDays).length
  // const onCheckBoxPress = () => {
  //   setError({ isVisible: false, type: "" })
  //   setAcceptedChecbox((prev) => !prev)
  // }
  const onBackNavigationPress = React.useCallback(() => {
    removeSelectedDays()
    removeSelectedWeeks()
    setAcceptedChecbox(false)
    navigation.goBack()
  }, [])
  const onNextButtonPress = async () => {
    if (error.isVisible) setError({ isVisible: false, type: "" })

    if (acceptedCheckbox && !validGoogleOAuth) {
      try {
        await startGoogleAuthentication(
          createNestedPath([
            DEEP_LINKING_URLS.NAVIGATION,
            DEEP_LINKING_URLS.HOME,
            DEEP_LINKING_URLS.AVAILABLE_DAYS_SELECTION,
          ])
        )
        setEventDate()
      } catch (e) {
        setError({ isVisible: true, type: "GoogleOauth" })
      }
    } else {
      setEventDate()
      setGCalEventsBooking(acceptedCheckbox)
      navigation.navigate("Available Time Selection")
    }
  }
  const setEventDate = React.useCallback(() => {
    const selectedDaysVal = Object.values(selectedDays).sort()
    setDateFrame(
      new Date(selectedDaysVal[0]),
      new Date(selectedDaysVal[selectedDaysVal.length - 1])
    )
  }, [selectedDays])

  const onHintClose = async () => {
    try {
      const userSettings: UserSettings = await getFromEncryptedStorage("user-settings")
      await setToEncryptedStorage("user-settings", {
        ...userSettings,
        eventCreationHintHidden: true,
      })
    } catch (e) {}
  }

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode ? Colors.primary.neutral : Colors.neutral.s600,
        },
      ]}>
      <View style={{ flex: 1, width: "100%", alignItems: "center" }}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <View style={{ width: "90%" }}>
          <HeaderText
            customStyles={{ marginBottom: Sizing.x10 }}
            colorScheme={colorScheme}>
            {`Select date${eventType === "one-time" ? "" : "s"} you are available`}
          </HeaderText>
        </View>
        {hintBoxVisible && eventType === "recurring" && (
          <HintBox
            text="Tap a weekday name to select all similar days in this month!"
            closeable={true}
            closeCallback={onHintClose}
          />
        )}
        <ScrollView showsVerticalScrollIndicator={false}>
          <CalendarWrapper>
            <MonthlyWrapper isNewEventCalendar={true} />
          </CalendarWrapper>
          {/*
          //TODO after beta release
          <View style={styles.messageWrapper}>
            <Checkbox
              onCheckBoxPress={onCheckBoxPress}
              acceptedCheckbox={acceptedCheckbox}>
              <BodyText
                customStyle={{
                  fontFamily: "Roboto-Medium",
                  fontSize: Sizing.x15,
                  width: "90%",
                }}
                changingColorScheme
                colors={[Colors.primary.s800, Colors.primary.neutral]}>
                Allow people to schedule my time on my Google calendar.
                {!validGoogleOAuth && !isInitialRequesting && (
                  <>
                    {" "}
                    <Text style={{ fontWeight: "bold" }}>Next:</Text> Grant access
                  </>
                )}
              </BodyText>
            </Checkbox>
          </View>
          */}
        </ScrollView>
        <FullWidthButton
          text="Next"
          colorScheme={colorScheme}
          disabled={isDisabledBtn}
          onPressCallback={onNextButtonPress}
          loadingIndicator={isRequesting}
          buttonType="filled"
          style={styles.button}
        />
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    marginVertical: Sizing.x15,
    alignSelf: "center",
    width: "90%",
  },
  messageWrapper: {
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    marginTop: Sizing.x10,
    alignItems: "center",
  },
  button: { width: "90%", marginTop: "auto", marginBottom: Sizing.x15 },
})
