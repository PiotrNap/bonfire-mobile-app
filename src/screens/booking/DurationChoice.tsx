import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { appContext, bookingContext, walletContext } from "contexts/contextApi"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { formatTimeIncrements, getRandomKey, getTimeSpanLength } from "lib/utils"
import { BookingStackParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"
import {
  assetsToUnitsArray,
  hexToUtf8,
  lovelaceToAda,
  schemaToPaymentTokens,
} from "lib/wallet/utils"
import { EventSlot } from "common/interfaces/newEventInterface"
import { HourlyRate, TimeIncrement } from "common/interfaces/bookingInterface"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { subHeader } from "../../styles/typography"

type Props = StackScreenProps<BookingStackParamList, "Duration Choice">

export const DurationChoice = ({ navigation, route }: Props) => {
  const { title, image, eventCardColor, eventTitleColor, availabilities, hourlyRate } =
    route.params?.event
  const { pickedStartTime, pickedDateSlots, pickedDateSlotsMinDuration, pickedDate } =
    bookingContext()
  const { lovelaceBalance, walletAssets } = walletContext()
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [selectedDuration, setSelectedDuration] = React.useState<number>(0)
  const [cost, setCost] = React.useState<HourlyRate>(new Map())
  const [eventHourlyRate, setEventHourlyRate] = React.useState<HourlyRate>(new Map())
  const [timeIncrements, setTimeIncrements] = React.useState<TimeIncrement[]>([])
  const { setDuration, setDurationCost } = bookingContext()
  const { colorScheme } = appContext()

  const hasEnoughFunds = React.useMemo(() => {
    if (!lovelaceBalance) return false
    if (cost.get("lovelace") === 0) return true

    const enoughLovalece = lovelaceBalance > cost.get("lovelace")
    for (let k of cost.keys()) {
      if (k === "lovelace") continue
      if (!walletAssets.get(k)) return false
      if (cost.get(k).count > walletAssets.get(k).count) return false
    }
    return enoughLovalece
  }, [walletAssets, lovelaceBalance, cost])
  const isLightMode = colorScheme === "light"
  const isDisabled = !selectedDuration || !hasEnoughFunds

  const setEventCost = (time: number) => {
    const multiplier = time / (1000 * 60 * 60)
    const newMapEntries = eventHourlyRate.entries()
    const newMap = new Map(newMapEntries)
    for (let k of newMap.keys()) {
      let v = newMap.get(k)

      if (k === "lovelace") {
        if (time === selectedDuration) v = 0
        newMap.set(k, Number(v) * multiplier)
      } else {
        if (typeof v !== "object") continue
        if (time === selectedDuration) {
          newMap.set(k, {
            ...v,
            count: 0,
          })
        } else {
          newMap.set(k, {
            ...v,
            count: Number(v?.count) * multiplier,
          })
        }
      }
    }
    setCost(newMap)
  }

  React.useEffect(() => {
    // calculate what duration can user book
    let _timeSlots: string[] = []
    let indexOfEventSlotArray = 0
    let indexOfEventSlot = 0
    let limitReached = false

    if (!pickedDateSlots || !pickedDateSlots.length) return
    // find first array of which first item contains selected date
    pickedDateSlots
      .find((slots: EventSlot[], idx) => {
        indexOfEventSlot = slots.findIndex((slot) => slot.from === pickedStartTime)
        if (indexOfEventSlot > -1) {
          indexOfEventSlotArray = idx
          return true
        }
        return false
      })
      ?.forEach((slot, idx) => {
        if (limitReached) return

        if (idx < indexOfEventSlot) {
          return
        } else if (!slot.isAvailable) {
          limitReached = true
        } else {
          _timeSlots.push(slot.from)
        }
        return
      })
    const { lovelace, assets } = schemaToPaymentTokens(hourlyRate)
    const units = assetsToUnitsArray([assets])

    //@ts-ignore
    const hourlyRateUnits = new Map([["lovelace", lovelace], ...units])
    setEventHourlyRate(hourlyRateUnits)

    let _timeIncrements = formatTimeIncrements(
      _timeSlots,
      pickedDateSlotsMinDuration[indexOfEventSlotArray]
    )
    setTimeIncrements(_timeIncrements)
  }, [hourlyRate, pickedStartTime, pickedDateSlots])

  React.useEffect(() => {
    setEventCost(0)
  }, [eventHourlyRate])

  const onNextPress = () => {
    //@TODO after beta release point user to deposit more funds if necessary
    //... or hide the time items that are too expensive
    // if (buttonText === "Deposit Funds")
    //   navigation.navigate("Add Funds", {
    //     ...route.params,
    //     fromScreen: "Duration Choice",
    //   })
    // if (buttonText === "Preview Order") {
    setDuration(selectedDuration)
    setDurationCost(cost)
    // transform PaymentTokens back into json schema but for a single date

    navigation.navigate("Booking Confirmation", {
      header: "Booking Details",
      event: route.params.event,
    })
    // }
  }

  const onBackNavigationPress = () => navigation.goBack()
  const onPressCallback = (timeIncrement: TimeIncrement, index: number) => {
    setSelectedDuration((p) =>
      p === timeIncrement.millSeconds ? 0 : timeIncrement.millSeconds
    )
    setEventCost(timeIncrements[index].millSeconds)
  }

  const renderTimeIncrements = (timeIncrement: TimeIncrement, index: number) => (
    <Pressable
      onPress={() => onPressCallback(timeIncrement, index)}
      hitSlop={5}
      key={getRandomKey(4)}
      style={[
        styles.timeSlotButton,
        selectedDuration === timeIncrements[index].millSeconds && {
          backgroundColor: Colors.primary.s600,
        },
      ]}>
      <Text
        style={[
          styles.timeSlotButtonText,
          selectedDuration === timeIncrements[index].millSeconds && {
            color: Colors.primary.neutral,
          },
        ]}>
        {timeIncrements[index].hours}
      </Text>
    </Pressable>
  )

  return (
    <EventBookingLayout
      onBackPress={onBackNavigationPress}
      screenHeader={"Select Duration"}
      screenSubHeader={
        ""
        // numOfSelectedDates > 1
        //   ? `You're creating a booking for ${numOfSelectedDates} dates.`
        //   : `You're creating a booking for ${numOfSelectedDates} date.`
      }
      eventCardColor={eventCardColor}
      eventCardImage={image}
      eventCardTitle={title}
      eventCardTitleColor={eventTitleColor}>
      <View style={styles.estimatedCostContainer}>
        <View style={styles.selectedCostWrapper}>
          {[...cost].map(([k, v]) =>
            k === "lovelace" ? (
              <Text
                key={getRandomKey(3)}
                style={isLightMode ? styles.eventCost_light : styles.eventCost_dark}>
                {lovelaceToAda(BigInt(v) || 0)} ₳
              </Text>
            ) : (
              <Text
                key={getRandomKey(3)}
                style={isLightMode ? styles.eventCost_light : styles.eventCost_dark}>
                {v.count} {hexToUtf8(v.name)}
              </Text>
            )
          )}
        </View>
        {/*
        <View style={styles.multiplier}>
          <Text style={isLightMode ? styles.eventCost_light : styles.eventCost_dark}>
            x {numOfSelectedDates} =
          </Text>
        </View>
        <View style={styles.selectedCostWrapper}>
          <Text style={isLightMode ? styles.eventCost_light : styles.eventCost_dark}>
            {lovelaceToAda(BigInt(cost.lovelace || 0) * BigInt(numOfSelectedDates))} ₳
          </Text>
          {Object.keys(cost).map((k) =>
            k === "lovelace" ? (
              <></>
            ) : (
              <Text
                key={getRandomKey(3)}
                style={isLightMode ? styles.eventCost_light : styles.eventCost_dark}>
                {cost[k].count * numOfSelectedDates} {hexToUtf8(cost[k].name)}
              </Text>
            )
          )}
        </View>
        */}
        {/*
        <Text
          style={isLightMode ? styles.walletBalance_light : styles.walletBalance_dark}>
          Available balance: {lovelaceToAda(lovelaceBalance).toFixed(2)} ₳
        </Text>
        */}
      </View>
      <View style={styles.timeSlotsContainer}>
        {timeIncrements && timeIncrements.map(renderTimeIncrements)}
      </View>
      <View style={styles.errorMessage}>
        {!hasEnoughFunds && (
          <SubHeaderText customStyle={{ ...subHeader.x20 }} colors={[Colors.danger.s400]}>
            You don't have enough funds for this duration
          </SubHeaderText>
        )}
      </View>
      <View style={styles.buttonContainer}>
        <FullWidthButton
          onPressCallback={onNextPress}
          text={"Preview Details"}
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
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    marginVertical: Sizing.x10,
  },
  selectedCostWrapper: {
    flexDirection: "column",
    alignItems: "center",
  },
  multiplier: { marginHorizontal: Sizing.x10 },
  errorMessage: {
    height: Sizing.x20,
    justifyContent: "center",
  },
  eventCost_light: {
    textAlignVertical: "center",
    fontSize: Sizing.x35,
    fontFamily: "Roboto-Bold",
    color: Colors.primary.s600,
  },
  eventCost_dark: {
    textAlignVertical: "center",
    fontSize: Sizing.x35,
    fontFamily: "Roboto-Bold",
    color: Colors.primary.neutral,
  },
  walletBalance_light: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s800,
  },
  walletBalance_dark: {
    ...Typography.subHeader.x10,
    color: Colors.primary.neutral,
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
