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
  onPressCallback: () => any | Promise<void>
  text: string
  loadingIndicatorStyle?: ViewStyle
  loadingIndicator?: boolean
  isOnboarding?: boolean
  colorScheme?: "light" | "dark"
  disabled?: boolean
  buttonType?: "filled" | "transparent"
  style?: StyleProp<any>
  textStyle?: StyleProp<any>
  lightMode?: boolean
}

export const FullWidthButton = ({
  colorScheme,
  onPressCallback,
  loadingIndicator,
  loadingIndicatorStyle,
  text,
  disabled,
  buttonType = "filled",
  isOnboarding = false,
  style,
  textStyle,
  lightMode,
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
  const textStyleFilled = [
    isLightMode ? Buttons.barText.primary_light : Buttons.barText.primary_dark,
  ]
  const textStyleTransparent = [
    isLightMode
      ? Buttons.barText.transparent_light
      : Buttons.barText.transparent_dark,
  ]
  const customButtonStyle =
    buttonType === "filled" ? buttonStyleFilled : buttonStyleTransparent
  const customTextStyle =
    buttonType === "filled" ? textStyleFilled : textStyleTransparent
  const disabledStyle = { opacity: 0.75 }

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
          ? [customButtonStyle, style, disabledStyle]
          : Buttons.applyOpacity([customButtonStyle, style])
      }>
      <Text onLayout={onLayoutText} style={[customTextStyle, textStyle]}>
        {text}
      </Text>
      {loadingIndicator && pressableWidth && textWidth && (
        <ActivityIndicator
          color={
            isOnboarding
              ? Colors.primary.neutral
              : isLightMode
              ? Colors.primary.neutral
              : Colors.primary.s800
          }
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
