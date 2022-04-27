import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { ClockIcon, RemoveIcon } from "assets/icons"
import { EventAvailability } from "common/interfaces/newEventInterface"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { getDigitalLocaleTime } from "lib/utils"
import { fontWeight } from "../../styles/typography"

export interface AvailabilityProps {
  availability: EventAvailability
  index: number
  onRemovePress: (index: number) => void
}

export const Availability = ({
  availability,
  onRemovePress,
  index,
}: AvailabilityProps) => {
  const { from, to, maxDuration, minDuration } = availability

  return (
    <View style={styles.container}>
      <View style={styles.cardItem}>
        <ClockIcon style={styles.clockIcon} strokeWidth={2} />
        <View style={styles.body}>
          <Text style={[styles.innerText, { ...fontWeight.semibold }]}>
            Start {getDigitalLocaleTime(from)} / End {getDigitalLocaleTime(to)}
          </Text>
          <Text style={[styles.innerText, { ...fontWeight.semibold }]}>
            Min. {minDuration} min / Max. {maxDuration} min
          </Text>
        </View>
        <Pressable
          hitSlop={5}
          onPress={() => onRemovePress(index)}
          style={Buttons.applyOpacity(styles.removeButton)}>
          <RemoveIcon style={styles.removeIcon} strokeWidth={2} />
        </Pressable>
      </View>
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    alignItems: "center",
    marginBottom: Sizing.x12,
  },
  cardItem: {
    width: "95%",
    paddingHorizontal: Sizing.x14,
    paddingVertical: Sizing.x10,
    flexDirection: "row",
    alignItems: "center",
    borderRadius: Outlines.borderRadius.max,
    borderColor: Colors.primary.s300,
    borderWidth: Outlines.borderWidth.thick,
    backgroundColor: Colors.primary.s180,
    ...Outlines.shadow.base,
  },
  body: {
    flex: 1,
    marginHorizontal: Sizing.x8,
  },
  innerText: {
    ...Typography.subHeader.x25,
    color: Colors.primary.s800,
  },
  removeButton: {
    width: Sizing.x40,
    height: Sizing.x40,
    borderRadius: Outlines.borderRadius.max,
    margin: Sizing.x5,
    alignItems: "center",
    justifyContent: "center",
  },
  removeIcon: {
    width: Sizing.x25,
    height: Sizing.x25,
    color: Colors.danger.s300,
    // color: Colors.neutral.s100,
  },
  clockIcon: {
    width: Sizing.x25,
    height: Sizing.x25,
    color: Colors.primary.s800,
  },
})
