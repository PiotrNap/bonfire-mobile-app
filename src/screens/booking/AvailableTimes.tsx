import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import {
  appContext,
  bookingContext,
  myCalendarContext,
} from "contexts/contextApi"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useAvailabilities } from "lib/hooks/useAvailabilities"
import { getDigitalLocaleTime } from "lib/utils"
import { BookingStackParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"
import { EventBookingLayout } from "components/layouts/eventBookingLayout"
import dayjs from "dayjs"

export interface AvailableTimesProps {}

type Props = StackScreenProps<BookingStackParamList, "Available Times">

export const AvailableTimes = ({ navigation, route }: Props) => {
  const { title, image, color, titleColor } = route.params
  const [selectedTimeSlot, setSelectedTimeSlot] = React.useState<number | null>(
    null
  )
  const {
    previewingEvent,
    pickedDate,
    setPickedDate,
    setMaxTimeSlotDuration,
    setMinTimeSlotDuration,
  } = bookingContext()
  const { colorScheme } = appContext()
  const { availabilities } = myCalendarContext()
  const { currAvailabilities } = useAvailabilities(
    previewingEvent.availabilities,
    pickedDate
  )

  // TODO once we want to adjust availabilities based on organizer scheduled time
  // const { scheduledTimes } = useScheduledTimes(
  //   events,
  //   pickedDate,
  //   previewingOrganizer.timeBlock
  // );
  const scheduledTimes: any = []
  const isDisabled = selectedTimeSlot === null

  const setTimeDuration = () => {
    if (selectedTimeSlot && currAvailabilities != null) {
      const availability = previewingEvent.availabilities.find(
        (availability: any) => {
          const from = new Date(availability.from).getTime()
          const to = new Date(availability.to).getTime()

          return from <= selectedTimeSlot && to > selectedTimeSlot
        }
      )

      setMaxTimeSlotDuration(availability.maxDuration)
      setMinTimeSlotDuration(availability.minDuration)
    }
  }

  const onBackNavigationPress = () =>
    navigation.navigate("Available Event Days Selection", route.params)
  const onNextPress = React.useCallback(() => {
    if (selectedTimeSlot && pickedDate) {
      const newDate = dayjs(pickedDate)
        .set("hour", dayjs(selectedTimeSlot).hour())
        .set("minutes", dayjs(selectedTimeSlot).minute())
        .toISOString()

      setPickedDate(new Date(newDate).getTime())
      setTimeDuration()
      navigation.navigate("Duration Choice", route.params)
    }
  }, [selectedTimeSlot, pickedDate])

  const onPressCallback = (item: number) => {
    if (scheduledTimes?.includes(item)) return
    if (selectedTimeSlot === item) return setSelectedTimeSlot(null)

    setSelectedTimeSlot(item)
  }

  const renderTimeSlots = (item: number, index: number) => {
    var _key = `${index}_${item}`
    return (
      <Pressable
        onPress={() => onPressCallback(item)}
        hitSlop={5}
        key={_key}
        style={[
          styles.timeSlotButton,
          scheduledTimes?.includes(item)
            ? { backgroundColor: Colors.booked }
            : {
                ...Outlines.shadow.lifted,
              },
          selectedTimeSlot === item && {
            backgroundColor: Colors.primary.s600,
          },
        ]}>
        <Text
          style={[
            styles.timeSlotButtonText,
            selectedTimeSlot === item && {
              color: Colors.available,
            },
          ]}>
          {getDigitalLocaleTime(item, "en") ?? {}}
        </Text>
      </Pressable>
    )
  }

  return (
    <EventBookingLayout
      onBackPress={onBackNavigationPress}
      screenHeader={"Select available time"}
      eventCardColor={color}
      eventCardImage={image}
      eventCardTitle={title}
      eventCardTitleColor={titleColor}>
      <View style={styles.timeSlotsContainer}>
        {availabilities && currAvailabilities?.map(renderTimeSlots)}
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
    marginHorizontal: Sizing.x5,
    borderRadius: Outlines.borderRadius.large,
  },
  timeSlotButtonText: {
    ...Typography.header.x35,
    color: Colors.primary.s800,
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
