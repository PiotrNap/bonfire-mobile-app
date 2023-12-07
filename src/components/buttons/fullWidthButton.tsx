import * as React from "react"
import {
  Pressable,
  Text,
  StyleProp,
  ActivityIndicator,
  LayoutChangeEvent,
  ViewStyle,
} from "react-native"

import { Buttons, Colors } from "styles/index"

export interface FullWidthButtonProps {
  onPressCallback: any
  text: string
  loadingIndicatorStyle?: ViewStyle
  loadingIndicator?: boolean
  isOnboarding?: boolean
  colorScheme?: "light" | "dark"
  disabled?: boolean
  buttonType?: "filled" | "transparent" | "neutral"
  style?: StyleProp<any>
  textStyle?: StyleProp<any>
  lightMode?: boolean
  customBgColor?: string
}

export const FullWidthButton = ({
  colorScheme,
  onPressCallback,
  loadingIndicator,
  loadingIndicatorStyle,
  text,
  disabled,
  buttonType = "filled",
  style,
  textStyle,
  lightMode,
  customBgColor,
}: FullWidthButtonProps) => {
  const [textWidth, setTextWidth] = React.useState<number>(0)
  const [pressableWidth, setPressableWidth] = React.useState<number>(0)
  const isLightMode = lightMode ?? colorScheme === "light"

  if (Array.isArray(style)) {
    style = Object.assign({}, ...style)
  }

  const buttonStyleFilled = isLightMode
    ? Buttons.bar.primary_light
    : Buttons.bar.primary_dark
  const buttonStyleTransparent = isLightMode
    ? Buttons.bar.transparent_light
    : Buttons.bar.transparent_dark
  const buttonStyleNeutral = {
    ...Buttons.bar.primary_dark,
    backgroundColor: Colors.primary.neutral,
  }
  const textStyleFilled = [
    isLightMode ? Buttons.barText.primary_light : Buttons.barText.primary_dark,
  ]
  const textStyleTransparent = [
    isLightMode ? Buttons.barText.transparent_light : Buttons.barText.transparent_dark,
  ]
  const textStyleNeutral = {
    ...Buttons.barText.primary_dark,
    color: Colors.primary.s800,
  }
  const customButtonStyle =
    buttonType === "filled"
      ? buttonStyleFilled
      : buttonType === "transparent"
      ? buttonStyleTransparent
      : buttonStyleNeutral
  const customTextStyle =
    buttonType === "filled"
      ? textStyleFilled
      : buttonType === "transparent"
      ? textStyleTransparent
      : textStyleNeutral
  const disabledStyle = { opacity: 0.7 }

  const onLayoutText = (e: LayoutChangeEvent) => {
    setTextWidth(e.nativeEvent?.layout?.width)
  }
  const onLayoutPressable = (e: LayoutChangeEvent) => {
    setPressableWidth(e.nativeEvent?.layout?.width)
  }

  return (
    <Pressable
      onPress={onPressCallback}
      onLayout={onLayoutPressable}
      disabled={disabled}
      hitSlop={5}
      style={
        disabled
          ? [
              customButtonStyle,
              style,
              disabledStyle,
              customBgColor && {
                backgroundColor: customBgColor,
                borderColor: customBgColor,
              },
            ]
          : Buttons.applyOpacity([
              customButtonStyle,
              style,
              customBgColor && {
                backgroundColor: customBgColor,
                borderColor: customBgColor,
              },
            ])
      }>
      <Text onLayout={onLayoutText} style={[customTextStyle, textStyle]}>
        {text}
      </Text>
      {loadingIndicator && pressableWidth && textWidth && (
        <ActivityIndicator
          color={Colors.primary.neutral}
          size="small"
          animating={true}
          style={{
            position: "absolute",
            right: pressableWidth / 2 - textWidth / 2 - 25,
            ...loadingIndicatorStyle,
          }}
        />
      )}
    </Pressable>
  )
}
