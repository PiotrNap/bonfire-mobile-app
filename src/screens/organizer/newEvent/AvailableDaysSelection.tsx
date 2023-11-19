import * as React from "react"
import { View, StyleSheet, Pressable, ScrollView, Text } from "react-native"

import { LeftArrowIcon } from "assets/icons"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { SafeAreaView } from "react-native-safe-area-context"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { HeaderText } from "components/rnWrappers/headerText"
import { StackScreenProps } from "@react-navigation/stack"
import { EventCreationParamList } from "common/types/navigationTypes"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { showErrorToast } from "lib/helpers"
import { Calendar, DateData } from "react-native-calendars"
import { MarkedDates } from "react-native-calendars/src/types"

type Props = StackScreenProps<EventCreationParamList, "Available Days Selection">

export const AvailableDaysSelection = (props: Props) => {
  const navigation = props.navigation
  const { colorScheme } = appContext()
  const { selectedDates, setSelectedDates, setDateFrame } = eventCreationContext()
  const [error, setError] = React.useState<any>({ isVisible: false, type: "" })
  const [_selectedDates, _setSelectedDates] = React.useState<MarkedDates>(selectedDates)

  // const googleOauthCallback = () => {
  //   setGCalEventsBooking(acceptedCheckbox)
  //   navigation.navigate("Available Time Selection")
  // }
  // const { isRequesting, isInitialRequesting, startGoogleAuthentication } = useGoogleAuth(
  //   googleOauthCallback,
  // setError
  // )

  React.useEffect(() => {
    if (error.isVisible) showErrorToast(null, error.type)
  }, [error])

  const isLightMode = colorScheme === "light"
  const lightOrDarkColor = isLightMode ? Colors.primary.s800 : Colors.primary.neutral
  const isDisabledBtn = !Object.keys(_selectedDates)
  const onDayPress = (day: DateData) => {
    const newSelectedDates = { ..._selectedDates }

    if (newSelectedDates[day.dateString]) {
      delete newSelectedDates[day.dateString]
    } else {
      newSelectedDates[day.dateString] = { selected: true }
    }

    _setSelectedDates(newSelectedDates)
  }

  const onBackNavigationPress = () => navigation.goBack()
  const onNextButtonPress = async () => {
    if (error.isVisible) setError({ isVisible: false, type: "" })

    // if (acceptedCheckbox && !validGoogleOAuth) {
    //   try {
    //     await startGoogleAuthentication(
    //       createNestedPath([
    //         DEEP_LINKING_URLS.NAVIGATION,
    //         DEEP_LINKING_URLS.HOME,
    //         DEEP_LINKING_URLS.AVAILABLE_DAYS_SELECTION,
    //       ])
    //     )
    //     setEventDate()
    //   } catch (e) {
    //     setError({ isVisible: true, type: "GoogleOauth" })
    //   }
    // } else {
    // setGCalEventsBooking(acceptedCheckbox)
    const datesArr = Object.keys(_selectedDates).sort()
    const fromDate = datesArr[0]
    const toDate = datesArr[datesArr.length - 1]
    setSelectedDates(_selectedDates)
    setDateFrame(new Date(fromDate), new Date(toDate))
    navigation.navigate("Available Time Selection")
  }
  // const setEventDate = React.useCallback(() => {
  //   const selectedDaysVal = Object.values(selectedDays).sort()
  //   setDateFrame(
  //     new Date(selectedDaysVal[0]),
  //     new Date(selectedDaysVal[selectedDaysVal.length - 1])
  //   )
  // }, [selectedDays])

  // const onHintClose = async () => {
  //   try {
  //     const userSettings: UserSettings = await getFromEncryptedStorage("user-settings")
  //     await setToEncryptedStorage("user-settings", {
  //       ...userSettings,
  //       eventCreationHintHidden: true,
  //     })
  //   } catch (e) {}
  // }

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
            Select dates you are available
          </HeaderText>
        </View>
        {/*
        {hintBoxVisible && eventType === "recurring" && (
          <HintBox
            text="Tap a weekday name to select all similar days in this month!"
            closeable={true}
            closeCallback={onHintClose}
          />
        )}
        */}
        <View style={styles.calendarWrapper}>
          <Calendar
            theme={{
              calendarBackground: !isLightMode
                ? Colors.primary.neutral
                : Colors.neutral.s600,
              textSectionTitleColor: "#b6c1cd",
              textSectionTitleDisabledColor: "#d9e1e8",
              selectedDayBackgroundColor: Colors.primary.s800,
              selectedDayTextColor: lightOrDarkColor,
              todayTextColor: "#00adf5",
              dayTextColor: "#2d4150",
              textDisabledColor: "#d9e1e8",
              dotColor: "#00adf5",
              selectedDotColor: "#ffffff",
              arrowColor: lightOrDarkColor,
              disabledArrowColor: "#d9e1e8",
              monthTextColor: "blue",
              indicatorColor: "blue",
              textDayFontFamily: "Roboto-Regular",
              textMonthFontFamily: "Roboto-Bold",
              textDayHeaderFontFamily: "Roboto-Medium",
              textDayFontSize: Sizing.x20,
              textMonthFontSize: 16,
              textDayHeaderFontSize: 16,
            }}
            onDayPress={onDayPress}
            markedDates={_selectedDates}
            style={styles.calendar}
            // enableSwipeMonths
            // Other props and customization...
          />
        </View>
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
        <FullWidthButton
          text="Next"
          colorScheme={colorScheme}
          disabled={isDisabledBtn}
          onPressCallback={onNextButtonPress}
          loadingIndicator={false}
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
  calendar: {
    width: "100%",
    marginTop: Sizing.x5,
    paddingVertical: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.lifted,
  },
  calendarWrapper: {
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
