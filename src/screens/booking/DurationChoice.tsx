import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { appContext, bookingContext } from "contexts/contextApi"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { getTimeSpanLength } from "lib/utils"
import { ProfileContext } from "contexts/profileContext"
import { useDurationSlots } from "lib/hooks/useDurationSlots"
import AnimatedNumber from "react-native-animated-number"
import { BookingStackParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"

type Props = StackScreenProps<BookingStackParamList, "Duration Choice">

export const DurationChoice = ({ navigation, route }: Props) => {
  const { title, image, color, titleColor } = route.params
  const { maxTimeSlotDuration, minTimeSlotDuration } = bookingContext()
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [selectedDuration, setSelectedDuration] = React.useState<number>(0)
  const [cost, setCost] = React.useState<number>(0)
  const { walletBalance } = React.useContext(ProfileContext)
  const { setDuration, setDurationCost } = bookingContext()
  const { colorScheme } = appContext()

  const isLightMode = colorScheme === "light"
  const isDisabled = selectedDuration === 0
  const buttonText =
    walletBalance != null && walletBalance < cost
      ? "Deposit Funds"
      : "Preview Order"

  const { timeSlots } = useDurationSlots(
    minTimeSlotDuration,
    maxTimeSlotDuration
  )

  const onBackNavigationPress = () => navigation.goBack()

  const onNextPress = async () => {
    // if (buttonText === "Sign up") return; // @TODO must navigate to sign up screen
    if (buttonText === "Deposit Funds")
      navigation.navigate("Add Funds", {
        ...route.params,
        fromScreen: "Duration Choice",
      })
    if (buttonText === "Preview Order") {
      setIsLoading(true)
      setDuration(selectedDuration)
      setDurationCost(cost)
      setIsLoading(false)
      navigation.navigate("Booking Confirmation", route.params)
    }
  }

  const onPressCallback = (time: number) => {
    if (selectedDuration === time) {
      setSelectedDuration(0)
      setCost(0)
    } else {
      setSelectedDuration(time)

      // TODO What's the hourly rate for this event?
      const hourlyRate = 50
      const totalCost = (hourlyRate ?? 50) * (time / 60 / 60 / 1000)

      setCost(Math.round(totalCost))
    }
  }

  const renderTimeSlots = React.useCallback(
    (time: number, index: number) => (
      <Pressable
        onPress={() => onPressCallback(time)}
        hitSlop={5}
        key={`${time}_${index}`}
        style={[
          styles.timeSlotButton,
          selectedDuration === time && {
            backgroundColor: Colors.primary.s600,
          },
        ]}>
        <Text
          style={[
            styles.timeSlotButtonText,
            selectedDuration === time && {
              color: Colors.available,
            },
          ]}>
          {getTimeSpanLength(time)}
        </Text>
      </Pressable>
    ),
    [selectedDuration]
  )

  return (
    <EventBookingLayout
      onBackPress={onBackNavigationPress}
      screenHeader={"Select duration and confirm"}
      eventCardColor={color}
      eventCardImage={image}
      eventCardTitle={title}
      eventCardTitleColor={titleColor}>
      <View style={styles.estimatedCostContainer}>
        <View style={styles.estimatedCostWrapper}>
          <AnimatedNumber
            value={cost}
            style={[isLightMode ? styles.totalAda_light : styles.totalAda_dark]}
          />
          <Text
            style={isLightMode ? styles.totalAda_light : styles.totalAda_dark}>
            ₳
          </Text>
        </View>
        <Text
          style={
            isLightMode ? styles.walletBalance_light : styles.walletBalance_dark
          }>
          Available balance: {walletBalance} ₳
        </Text>
      </View>
      <View style={styles.timeSlotsContainer}>
        {timeSlots && timeSlots.map(renderTimeSlots)}
      </View>
      <View style={styles.buttonContainer}>
        <FullWidthButton
          onPressCallback={onNextPress}
          text={buttonText}
          colorScheme={colorScheme}
          disabled={isDisabled}
          loadingIndicator={isLoading}
        />
      </View>
    </EventBookingLayout>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  timesHeader: {
    marginTop: Sizing.x25,
    marginBottom: Sizing.x5,
    marginRight: "auto",
    marginLeft: Sizing.x25,
  },
  timesHeaderText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  timesHeaderText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
  buttonContainer: {
    alignItems: "center",
    justifyContent: "center",
    width: "90%",
    marginVertical: Sizing.x10,
  },
  timeSlotsContainer: {
    width: "90%",
    alignItems: "center",
    justifyContent: "space-evenly",
    flexDirection: "row",
    flexWrap: "wrap",
  },
  timeSlotButton: {
    width: "30%",
    alignItems: "center",
    backgroundColor: Colors.available,
    paddingVertical: Sizing.x3,
    paddingHorizontal: Sizing.x3,
    marginVertical: Sizing.x10,
    borderRadius: Outlines.borderRadius.large,
    ...Outlines.shadow.lifted,
  },
  timeSlotButtonText: {
    ...Typography.header.x35,
    color: Colors.primary.s800,
  },
  estimatedCostContainer: {
    alignItems: "center",
    justifyContent: "center",
    marginVertical: Sizing.x10,
  },
  estimatedCostWrapper: {
    flexDirection: "row",
    textAlign: "auto",
  },
  totalAda_light: {
    textAlignVertical: "center",
    fontSize: Sizing.x60,
    fontFamily: "Roboto-Medium",
    color: Colors.primary.s600,
  },
  totalAda_dark: {
    fontSize: Sizing.x60,
    fontFamily: "Roboto-Medium",
    textAlignVertical: "center",
    color: Colors.primary.neutral,
  },
  walletBalance_light: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s800,
  },
  walletBalance_dark: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s200,
  },
  topContainer: {
    height: Sizing.x100,
  },
  bottomContainer: {
    flexGrow: 1,
    alignItems: "center",
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  bottomWrapper: {
    flex: 1,
    width: "90%",
    paddingVertical: Sizing.x20,
    justifyContent: "space-between",
  },
  backgroundImage: {
    width: "100%",
    height: Sizing.x120,
    position: "absolute",
    top: 0,
  },
  topInnerContainer: {
    height: "100%",
    alignItems: "center",
    justifyContent: "flex-start",
    paddingBottom: Sizing.x15,
  },
  topInnerWrapper: {
    width: "90%",
    flexDirection: "row",
    justifyContent: "space-between",
  },
  eventTitleWrapper: {
    width: "90%",
    marginTop: "auto",
    marginBottom: Sizing.x20,
  },
  eventTitle: {
    ...Typography.header.x55,
    color: Colors.primary.neutral,
  },
})
