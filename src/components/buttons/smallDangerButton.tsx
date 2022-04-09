import { BodyText } from "components/rnWrappers/bodyText"
import { appContext } from "contexts/contextApi"
import * as React from "react"
import { StyleSheet, Pressable, ViewStyle } from "react-native"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { applyOpacity } from "../../styles/colors"

export interface Props {
  onPressCallback: () => void
  text: string
  buttonStyle?: ViewStyle
}

export const SmallDangerButton = ({
  onPressCallback,
  buttonStyle,
  text,
}: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"

  const btnStyle = isLightMode
    ? { ...styles.button, ...styles.button_light }
    : { ...styles.button, ...styles.button_dark }

  const btnTextStyle = isLightMode
    ? { ...styles.btnText, ...styles.buttonText_light }
    : { ...styles.btnText, ...styles.buttonText_dark }

  return (
    <Pressable
      hitSlop={5}
      onPress={onPressCallback}
      style={Buttons.applyOpacity({ ...btnStyle, ...buttonStyle })}>
      <BodyText changingColorScheme={false} customStyle={btnTextStyle}>
        {text}
      </BodyText>
    </Pressable>
  )
}

const styles = StyleSheet.create({
  button: {
    alignItems: "center",
    justifyContent: "center",
    flex: 1,
    padding: Sizing.x3,
    borderRadius: Outlines.borderRadius.small,
  },
  button_light: {
    backgroundColor: applyOpacity(Colors.danger.s300, 0.2),
    borderColor: Colors.danger.s400,
    borderWidth: Outlines.borderWidth.base,
  },
  button_dark: {
    backgroundColor: applyOpacity(Colors.danger.s300, 0.5),
    borderColor: applyOpacity(Colors.danger.s400, 0.8),
    borderWidth: Outlines.borderWidth.base,
  },
  btnText: {
    ...Typography.header.x20,
  },
  buttonText_light: {
    color: Colors.danger.s400,
  },
  buttonText_dark: {
    color: Colors.primary.neutral,
  },
})
