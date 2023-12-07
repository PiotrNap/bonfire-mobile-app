import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { appContext, bookingContext, myCalendarContext } from "contexts/contextApi"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useAvailabilities } from "lib/hooks/useAvailabilities"
import { getDigitalLocaleTime, getRandomKey } from "lib/utils"
import { BookingStackParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"
import { EventSlot } from "common/interfaces/newEventInterface"

export interface AvailableTimesProps {}

type Props = StackScreenProps<BookingStackParamList, "Available Times">

export const AvailableTimes = ({ navigation, route }: Props) => {
  const { title, image, eventCardColor, eventTitleColor } = route.params?.event
  const [selectedEventSlot, setSelectedEventSlot] = React.useState<EventSlot | null>(null)
  const { setPickedStartTime, pickedDateSlots } = bookingContext()
  const { colorScheme } = appContext()

  // const scheduledTimes: any = []
  const isDisabled = selectedEventSlot === null

  const onBackNavigationPress = () =>
    navigation.navigate("Available Event Dates Selection", route.params)
  const onNextPress = () => {
    if (!selectedEventSlot) return
    setPickedStartTime(selectedEventSlot.from)
    navigation.navigate("Duration Choice", route.params)
  }

  const onPressCallback = (item: EventSlot) => {
    // if (scheduledTimes?.includes(item)) return // whether or not given time slot is already booked
    if (selectedEventSlot === item) return setSelectedEventSlot(null)

    setSelectedEventSlot(item)
  }

  const renderEventSlot = (item: EventSlot) => {
    if (!item.isAvailable) return <></> // do not render unavailable slots

    return (
      <Pressable
        onPress={() => onPressCallback(item)}
        hitSlop={5}
        key={getRandomKey(3)}
        style={[
          styles.timeSlotButton,
          // scheduledTimes?.includes(item)
          //   ? { backgroundColor: Colors.booked }
          //   : {
          //       ...Outlines.shadow.lifted,
          //     },
          selectedEventSlot?.from === item.from && {
            backgroundColor: Colors.primary.s600,
          },
        ]}>
        <Text
          style={[
            styles.timeSlotButtonText,
            selectedEventSlot === item && {
              color: Colors.primary.neutral,
            },
          ]}>
          {getDigitalLocaleTime(item.from) ?? {}}
        </Text>
      </Pressable>
    )
  }

  const renderArrayOfPickedSlots = () => {
    if (!pickedDateSlots?.length) return <></>
    const children = []

    // sort by the time windows
    const sortedPickedDateSlots = pickedDateSlots
      .flat()
      .sort((a, b) => (a.from > b.from ? 1 : -1))

    for (let slot of sortedPickedDateSlots) {
      if (!slot) continue
      children.push(renderEventSlot(slot))
    }
    return children
  }

  return (
    <EventBookingLayout
      onBackPress={onBackNavigationPress}
      screenHeader={"Pick a Start Time"}
      eventCardColor={eventCardColor}
      eventCardImage={image}
      eventCardTitle={title}
      eventCardTitleColor={eventTitleColor}>
      <View style={styles.timeSlotsContainer}>{renderArrayOfPickedSlots()}</View>
      <View style={styles.buttonContainer}>
        <FullWidthButton
          onPressCallback={onNextPress}
          text={"Next"}
          colorScheme={colorScheme}
          disabled={isDisabled}
        />
      </View>
    </EventBookingLayout>
  )
}

const styles = StyleSheet.create({
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
    marginHorizontal: Sizing.x5,
    borderRadius: Outlines.borderRadius.large,
  },
  timeSlotButtonText: {
    ...Typography.header.x35,
    color: Colors.primary.s800,
  },
})
