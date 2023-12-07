import * as React from "react"
import { View, StyleSheet, Pressable } from "react-native"

import { useNavigation } from "@react-navigation/native"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { getRandomKey } from "lib/utils"
import { EventBookingSlot } from "common/types/dto"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { RightArrowIcon } from "assets/icons"
import { appContext } from "contexts/contextApi"

export interface SlotsListItemProps {
  item: EventBookingSlot
  slotType: "bookedSlots" | "scheduledSlots"
}

export const SlotsListItem = ({ item, slotType }: SlotsListItemProps) => {
  const { colorScheme } = appContext()
  const navigation = useNavigation()

  const onCardPress = () => {
    navigation.navigate("Confirmation Details", {
      bookingSlot: item,
      bookingSlotType: slotType,
    })
  }

  return (
    <Pressable
      key={getRandomKey(2)}
      onPress={onCardPress}
      style={Buttons.applyOpacity([
        styles.main,
        //TODO based on the slotType change the backgroundColor to either  SCHEDULED_COLOR_LIGHT/BOOKED_COLOR_LIGHT
      ])}>
      <View pointerEvents="none" style={styles.textContainer}>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          Event: {item.eventTitle}
        </SubHeaderText>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          {slotType === "bookedSlots"
            ? `By: ${item.organizerAlias}`
            : `With: ${item.attendeeAlias}`}
        </SubHeaderText>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          From: {new Date(item.fromDate).toLocaleString()}
        </SubHeaderText>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          To: {new Date(item.toDate).toLocaleString()}
        </SubHeaderText>
      </View>
      <View pointerEvents="none" style={styles.arrowContainer}>
        <RightArrowIcon
          width="20"
          height="20"
          strokeWidth={2}
          stroke={colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral}
          style={{ marginRight: "auto" }}
        />
      </View>
    </Pressable>
  )
}

const styles = StyleSheet.create({
  main: {
    flexDirection: "row",
    alignItems: "center",
    padding: Sizing.x10,
    marginVertical: Sizing.x10,
    ...Outlines.shadow.base,
    borderRadius: Outlines.borderRadius.base,
    backgroundColor: Colors.neutral.s500,
  },
  arrowContainer: {},
  textContainer: {
    flex: 1,
  },
})
