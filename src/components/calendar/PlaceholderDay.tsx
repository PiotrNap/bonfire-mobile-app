import * as React from "react"
import { Pressable, StyleSheet, Text, View } from "react-native"
import { Colors, Sizing, Typography } from "styles/index"

export interface PlaceholderDayProps {
  number: number
  key: string
  direction: string
  onPlaceholderPress: (direction: string) => void
}

const _PlaceholderDay = ({
  number,
  direction,
  onPlaceholderPress,
}: PlaceholderDayProps) => {
  const onPress = () => {
    onPlaceholderPress(direction)
  }

  return (
    <View style={styles.container}>
      <Pressable onPress={onPress} hitSlop={Sizing.x5} style={styles.dayButton}>
        <Text style={styles.placeholderNumber}>{number}</Text>
      </Pressable>
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    width: `${100 / 7}%`,
    height: `${100 / 6}%`,
    justifyContent: "center",
    alignItems: "center",
  },
  dayButton: {
    width: 33,
    height: 33,
    alignItems: "center",
    justifyContent: "center",
  },
  placeholderNumber: {
    ...Typography.body.x30,
    ...Typography.roboto.regular,
    color: Colors.primary.s350,
  },
})

export const PlaceholderDay = React.memo(_PlaceholderDay)
