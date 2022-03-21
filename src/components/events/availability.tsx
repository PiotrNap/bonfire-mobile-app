import * as React from "react";
import { View, Text, StyleSheet, Pressable } from "react-native";

import { ClockIcon, RemoveIcon } from "assets/icons";
import { EventAvailability } from "common/interfaces/newEventInterface";
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index";
import { getDigitalLocaleTime } from "lib/utils";
import { fontWeight } from "../../styles/typography";

export interface AvailabilityProps {
  availability: EventAvailability;
  index: number;
  onRemovePress: (index: number) => void;
}

export const Availability = ({
  availability,
  onRemovePress,
  index,
}: AvailabilityProps) => {
  const { from, to, maxDuration, minDuration } = availability;

  return (
    <View style={styles.container}>
      <View style={styles.cardItem}>
        <ClockIcon style={styles.clockIcon} strokeWidth={2} />
        <View style={styles.body}>
          <Text style={[styles.innerText, { ...fontWeight.semibold }]}>
            From {getDigitalLocaleTime(from)} - to {getDigitalLocaleTime(to)}
          </Text>
          <Text style={[styles.innerText, { ...fontWeight.semibold }]}>
            min. {minDuration} min / max. {maxDuration} min
          </Text>
        </View>
        <Pressable
          hitSlop={5}
          onPress={() => onRemovePress(index)}
          style={Buttons.applyOpacity(styles.removeButton)}>
          <RemoveIcon style={styles.trashIcon} strokeWidth={1.3} />
        </Pressable>
      </View>
    </View>
  );
};

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
    backgroundColor: Colors.primary.s400,
    ...Outlines.shadow.base,
  },
  body: {
    flex: 1,
    marginHorizontal: Sizing.x8,
  },
  innerText: {
    ...Typography.subHeader.x30,
    color: Colors.primary.s800,
  },
  removeButton: {
    width: Sizing.x40,
    height: Sizing.x40,
    backgroundColor: Colors.danger.s300,
    borderRadius: Outlines.borderRadius.max,
    margin: Sizing.x5,
    alignItems: "center",
    justifyContent: "center",
  },
  trashIcon: {
    width: Sizing.x25,
    height: Sizing.x25,
    color: Colors.neutral.s100,
  },
  clockIcon: {
    width: Sizing.x25,
    height: Sizing.x25,
    color: Colors.primary.s800,
  },
});
