/**
 * @description A wrapper around react native Text component
 * @param children - text node
 * @param colors - an array of two colors for light/dark mode
 */

import * as React from "react"
import { Text, StyleSheet, StyleProp } from "react-native"

import { appContext } from "contexts/contextApi"
import { Colors, Typography } from "styles/index"

export interface BodyTextProps {
  children?: React.ReactNode
  colors?: string[]
  changingColorScheme?: boolean
  customStyle?: StyleProp<any> | Array<StyleProp<any>>
}

export const BodyText = ({
  children,
  colors,
  customStyle,
  changingColorScheme,
}: BodyTextProps) => {
  const { colorScheme } = appContext()

  const textColor =
    changingColorScheme && colorScheme != null && colors
      ? colorScheme === "light"
        ? { color: colors[0] }
        : { color: colors[1] }
      : {
          color:
            colorScheme === "light"
              ? Colors.primary.s600
              : Colors.primary.neutral,
        }

  return <Text style={[styles.text, textColor, customStyle]}>{children}</Text>
}

const styles = StyleSheet.create({
  text: {
    ...Typography.body.x30,
  },
})
