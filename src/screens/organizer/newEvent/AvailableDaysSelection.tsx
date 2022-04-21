import * as React from "react"
import {
  View,
  StyleSheet,
  Pressable,
  ScrollView,
  Text,
  Linking,
} from "react-native"

import { CheckIcon, LeftArrowIcon } from "assets/icons"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { SafeAreaView } from "react-native-safe-area-context"
import { Colors, Outlines, Sizing } from "styles/index"
import { HeaderText } from "components/rnWrappers/headerText"
import { StackScreenProps } from "@react-navigation/stack"
import {
  DEEP_LINKING_PATHS,
  EventCreationParamList,
} from "common/types/navigationTypes"
import { MonthlyWrapper } from "components/calendar"
import { CalendarWrapperSimple } from "components/calendar/CalendarWrapperSimple"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { BodyText } from "components/rnWrappers/bodyText"
import { useGoogleAuth } from "lib/hooks/useGoogleAuth"
import { SlideTopModal } from "components/modals/slideTopModal"
import { createNestedPath } from "lib/navigation"
import { HintBox } from "components/modals/HintBox"
import { ErrorIcon } from "assets/icons"
import {
  getFromEncryptedStorage,
  setToEncryptedStorage,
} from "lib/encryptedStorage"
import { UserSettings } from "common/interfaces/appInterface"
import * as qs from "qs"

type Props = StackScreenProps<
  EventCreationParamList,
  "Available Days Selection"
>

export const AvailableDaysSelection = (props: Props) => {
  const navigation = props.navigation
  const { colorScheme } = appContext()
  const {
    selectedDays,
    setDateFrame,
    removeSelectedDays,
    removeSelectedWeeks,
  } = eventCreationContext()
  const {
    isRequesting,
    isValidOauth,
    setIsValidOauth,
    isLoading,
    startGoogleAuthentication,
  } = useGoogleAuth()
  const [acceptedCheckbox, setAcceptedChecbox] = React.useState<boolean>(false)
  const [hintBoxVisible, setHintBoxVisible] = React.useState<boolean>(false)
  const [error, setError] = React.useState<any>({ isVisible: false, type: "" })

  const eventListener = React.useCallback((event: { url: string }) => {
    const query = qs.parse(event.url.split("?")[1])
    const { success } = query

    if (success === "false") {
      setError({ isVisible: true, type: "GoogleOauth" })
    }

    setIsValidOauth(true)
    if (success === "true") navigation.navigate("Available Time Selection")
  }, [])

  React.useEffect(() => {
    ;(async () => {
      const userSettings: UserSettings = await getFromEncryptedStorage(
        "user-settings"
      )
      if (!userSettings?.eventCreationHintHidden) setHintBoxVisible(true)
    })()

    Linking.addEventListener("url", eventListener)
    return () => Linking.removeEventListener("url", eventListener)
  }, [])

  const isLightMode = colorScheme === "light"
  const isDisabledBtn =
    selectedDays === null || !Object.entries(selectedDays).length
  const onCheckBoxPress = () => {
    setError({ isVisible: false, type: "" })
    setAcceptedChecbox((prev) => !prev)
  }
  const onBackNavigationPress = React.useCallback(() => {
    removeSelectedDays()
    removeSelectedWeeks()
    setAcceptedChecbox(false)
    navigation.goBack()
  }, [])
  const onNextButtonPress = async () => {
    if (error.isVisible) setError({ isVisible: false, type: "" })

    if (acceptedCheckbox)
      try {
        await startGoogleAuthentication(
          createNestedPath([
            DEEP_LINKING_PATHS.NAVIGATION,
            DEEP_LINKING_PATHS.AVAILABLE_DAYS_SELECTION,
          ])
        )
      } catch (e) {}

    const selectedDaysVal = Object.values(selectedDays).sort()
    setDateFrame(
      new Date(selectedDaysVal[0]),
      new Date(selectedDaysVal[selectedDaysVal.length - 1])
    )

    if (!acceptedCheckbox) navigation.navigate("Available Time Selection")
  }

  const ErrorModalIcon = React.memo(() => (
    <ErrorIcon
      stroke={Colors.primary.neutral}
      width={Sizing.x60}
      height={Sizing.x60}
      strokeWidth={1.5}
    />
  ))

  const onHintClose = async () => {
    try {
      const userSettings: UserSettings = await getFromEncryptedStorage(
        "user-settings"
      )
      await setToEncryptedStorage("user-settings", {
        ...userSettings,
        eventCreationHintHidden: true,
      })
    } catch (e) {}
  }

  return (
    <>
      <SafeAreaView
        style={[
          styles.safeArea,
          {
            backgroundColor: isLightMode
              ? Colors.primary.neutral
              : Colors.neutral.s600,
          },
        ]}>
        <View style={{ flex: 1, width: "100%", alignItems: "center" }}>
          <View style={styles.navigation}>
            <Pressable onPress={onBackNavigationPress} hitSlop={10}>
              <LeftArrowIcon
                width={24}
                height={24}
                color={
                  isLightMode ? Colors.primary.s600 : Colors.primary.neutral
                }
              />
            </Pressable>
          </View>
          <View style={{ width: "90%" }}>
            <HeaderText
              customStyles={{ marginBottom: Sizing.x10 }}
              colorScheme={colorScheme}>
              Select dates you are available to host event
            </HeaderText>
          </View>
          {hintBoxVisible && (
            <HintBox
              text="Press on a day to
        select one, or tap on a day name to select them all."
              closeable={true}
              closeCallback={onHintClose}
            />
          )}
          <View style={styles.messageWrapper}></View>
          <ScrollView showsVerticalScrollIndicator={false}>
            <CalendarWrapperSimple>
              <MonthlyWrapper isNewEventCalendar={true} />
            </CalendarWrapperSimple>
            {!isValidOauth && !isLoading && (
              <View style={styles.messageWrapper}>
                <Pressable
                  onPress={onCheckBoxPress}
                  hitSlop={5}
                  style={[
                    styles.checkbox,
                    {
                      borderWidth: isLightMode ? Outlines.borderWidth.thin : 0,
                      backgroundColor:
                        isLightMode && acceptedCheckbox
                          ? Colors.primary.s600
                          : Colors.primary.neutral,
                      borderColor:
                        isLightMode && acceptedCheckbox
                          ? Colors.primary.s600
                          : "black",
                    },
                  ]}>
                  <CheckIcon
                    width="15"
                    height="15"
                    strokeWidth="3.5"
                    stroke={
                      isLightMode
                        ? Colors.primary.neutral
                        : !isLightMode && acceptedCheckbox
                        ? Colors.primary.s600
                        : Colors.primary.neutral
                    }
                  />
                </Pressable>
                <BodyText
                  customStyle={{
                    fontFamily: "Roboto-Regular",
                    fontSize: Sizing.x14,
                    width: "90%",
                  }}
                  changingColorScheme
                  colors={[Colors.primary.s800, Colors.primary.neutral]}>
                  Attendees schedule time on my Google calendar
                  {"\n"}
                  <Text style={{ fontWeight: "bold" }}>Next:</Text> Grant access
                </BodyText>
              </View>
            )}
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
      <SlideTopModal
        icon={ErrorModalIcon}
        isModalVisible={error.isVisible}
        modalContent={error.type}
        backgroundColor={Colors.danger.s300}
      />
    </>
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
    marginTop: Sizing.x5,
  },
  checkbox: {
    alignItems: "center",
    justifyContent: "center",
    width: 17,
    height: 17,
    marginTop: Sizing.x5,
    marginRight: Sizing.x10,
    marginLeft: Sizing.x2,
    borderRadius: Sizing.x3,
  },
  button: { width: "90%", marginTop: "auto", marginBottom: Sizing.x15 },
})
