import { RightArrowIcon } from "assets/icons"
import { appContext } from "contexts/contextApi"
import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"
import { Colors, Sizing, Typography } from "styles/index"

export interface Props {
  icon: any
  onPressCallback: () => void
  title: string
}

export const SettingsNavigationItem = ({
  onPressCallback,
  title,
  icon,
}: Props) => {
  const { colorScheme } = appContext()
  const IconComponent = icon

  return (
    <Pressable style={styles.navigationItem} onPress={onPressCallback}>
      <IconComponent
        width={26}
        height={26}
        color={
          colorScheme === "light"
            ? Colors.primary.brand
            : Colors.primary.neutral
        }
        strokeWidth={2}
        style={styles.icon}
      />
      <Text
        style={
          colorScheme === "light"
            ? styles.navigationItemText_light
            : styles.navigationItemText_dark
        }>
        {title}
      </Text>
      <RightArrowIcon
        width={26}
        height={26}
        color={
          colorScheme === "light"
            ? Colors.primary.brand
            : Colors.primary.neutral
        }
        style={styles.navigationItemIcon}
        strokeWidth={1.6}
      />
    </Pressable>
  )
}

const styles = StyleSheet.create({
  navigationItem: {
    flexDirection: "row",
    alignItems: "center",
    width: "85%",
    marginBottom: Sizing.x20,
  },
  navigationItemIcon: {
    marginLeft: "auto",
  },
  navigationItemText_light: {
    ...Typography.subHeader.x30,
    color: Colors.primary.s600,
  },
  navigationItemText_dark: {
    ...Typography.subHeader.x30,
    color: Colors.primary.neutral,
  },
  icon: {
    marginRight: Sizing.x10,
  },
})
