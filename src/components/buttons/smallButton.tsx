import * as React from "react"
import { Text, StyleSheet, Pressable } from "react-native"

import { Outlines, Buttons, Typography, Colors, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import { ColorSchemeName } from "common/interfaces/appInterface"

export interface Props {
  onPress: () => void
  icon?: React.ReactNode
  title?: string
  customStyle?: any
  customColorScheme?: ColorSchemeName
  children?: React.ReactNode
}

export const SmallButton = ({
  onPress,
  icon,
  title,
  customStyle,
  customColorScheme,
  children,
}: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = (customColorScheme || colorScheme) === "light"

  return (
    <Pressable
      onPress={onPress}
      style={Buttons.applyOpacity(
        Object.assign(
          {},
          styles.button,
          isLightMode
            ? { backgroundColor: Colors.primary.s800 }
            : { backgroundColor: Colors.primary.s600 },
          icon ? styles.withIcon : {},
          customStyle
        )
      )}>
      {children ? (
        children
      ) : (
        <>
          {title && <Text style={styles.buttonText}>{title}</Text>}
          {icon}
        </>
      )}
    </Pressable>
  )
}

const styles = StyleSheet.create({
  button: {
    borderRadius: Outlines.borderRadius.base,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    paddingVertical: Sizing.x5,
    paddingHorizontal: Sizing.x10,
    ...Outlines.shadow.base,
  },
  buttonText: {
    ...Typography.header.x20,
    color: Colors.primary.neutral,
  },
  withIcon: {
    marginRight: Sizing.x5,
  },
})
