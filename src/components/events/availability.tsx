import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { ClockIcon, RemoveIcon } from "assets/icons"
import { EventAvailability } from "common/interfaces/newEventInterface"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { getLocaleTimezone } from "lib/utils"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"

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
          <SubHeaderText colors={[Colors.primary.s800]} customStyle={styles.innerText}>
            {`${from.hour}:${from.minutes === 0 ? "00" : from.minutes} - ${to.hour}:${
              to.minutes === 0 ? "00" : to.minutes
            } ${getLocaleTimezone()}`}
          </SubHeaderText>
          <SubHeaderText colors={[Colors.primary.s800]} customStyle={styles.innerText}>
            Min. {minDuration} min / Max. {maxDuration} min
          </SubHeaderText>
        </View>
        <Pressable
          hitSlop={Sizing.x10}
          style={Buttons.applyOpacity(styles.removeButton)}
          onPress={() => onRemovePress(index)}>
          <RemoveIcon style={styles.icon} stroke={Colors.neutral.s150} />
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
    paddingVertical: Sizing.x8,
    paddingHorizontal: Sizing.x10,
    flexDirection: "row",
    alignItems: "center",
    borderRadius: Outlines.borderRadius.max,
    borderColor: Colors.primary.s300,
    borderWidth: Outlines.borderWidth.base,
    backgroundColor: Colors.primary.s180,
    ...Outlines.shadow.base,
  },
  body: {
    flex: 1,
    marginHorizontal: Sizing.x8,
  },
  innerText: {
    flex: 1,
    ...Typography.subHeader.x10,
    fontFamily: "Roboto-Medium",
    paddingLeft: Sizing.x5,
  },
  removeButton: {
    width: Sizing.x30,
    height: Sizing.x30,
    backgroundColor: Colors.danger.s400,
    alignItems: "center",
    justifyContent: "center",
    borderRadius: Outlines.borderRadius.max,
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
  },
  clockIcon: {
    width: Sizing.x25,
    height: Sizing.x25,
    color: Colors.primary.s800,
  },
})
