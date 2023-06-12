import * as React from "react"
import { StyleSheet, Pressable } from "react-native"

import { Outlines, Buttons, Colors, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"

export interface Props {
  onPress: () => void
  icon: React.ReactNode
}

export const RoundedButton = ({ onPress, icon }: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  return (
    <Pressable
      onPress={onPress}
      style={Buttons.applyOpacity(
        Object.assign(
          {},
          styles.button,
          isLightMode
            ? { backgroundColor: Colors.primary.s800 }
            : { backgroundColor: Colors.primary.s600 }
        )
      )}>
      {icon}
    </Pressable>
  )
}

const styles = StyleSheet.create({
  button: {
    borderRadius: Outlines.borderRadius.max,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    padding: Sizing.x7,
    ...Outlines.shadow.base,
  },
})
