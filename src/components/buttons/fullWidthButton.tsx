import * as React from "react";
import {
  Pressable,
  Text,
  StyleProp,
  ActivityIndicator,
  LayoutChangeEvent,
} from "react-native";

import { Buttons, Colors } from "styles/index";

export interface FullWidthButton {
  onPressCallback: () => any | Promise<void>;
  text: string;
  loadingIndicator?: boolean;
  colorScheme?: "light" | "dark";
  disabled?: boolean;
  buttonType?: "filled" | "transparent";
  style?: StyleProp<any>;
  textStyle?: StyleProp<any>;
  lightMode?: boolean;
}

export const FullWidthButton = ({
  colorScheme,
  onPressCallback,
  loadingIndicator,
  text,
  disabled,
  buttonType = "filled",
  style,
  textStyle,
  lightMode,
}: FullWidthButton) => {
  const [textWidth, setTextWidth] = React.useState<number>(0);
  const [pressableWidth, setPressableWidth] = React.useState<number>(0);
  const isLightMode = lightMode ?? colorScheme === "light";

  if (Array.isArray(style)) {
    style = Object.assign({}, ...style);
  }

  const disabledStyle =
    buttonType === "filled"
      ? { backgroundColor: Colors.neutral.s400 }
      : { opacity: 0.65 };

  const buttonStyleFilled = [
    isLightMode ? Buttons.bar.primary_light : Buttons.bar.primary_dark,
  ];
  const buttonStyleTransparent = [
    isLightMode ? Buttons.bar.transparent_light : Buttons.bar.transparent_dark,
  ];

  const textStyleFilled = [
    isLightMode ? Buttons.barText.primary_light : Buttons.barText.primary_dark,
  ];
  const textStyleTransparent = [
    isLightMode
      ? Buttons.barText.transparent_light
      : Buttons.barText.transparent_dark,
  ];

  const customButtonStyle =
    buttonType === "filled" ? buttonStyleFilled : buttonStyleTransparent;
  const customTextStyle =
    buttonType === "filled" ? textStyleFilled : textStyleTransparent;

  const onLayoutText = (e: LayoutChangeEvent) => {
    setTextWidth(e.nativeEvent?.layout?.width);
  };

  const onLayoutPressable = (e: LayoutChangeEvent) => {
    setPressableWidth(e.nativeEvent?.layout?.width);
  };

  return (
    <Pressable
      onPress={onPressCallback}
      onLayout={onLayoutPressable}
      disabled={disabled}
      hitSlop={5}
      style={
        disabled
          ? Object.assign({}, ...customButtonStyle, style, disabledStyle)
          : Buttons.applyOpacity([
              Object.assign({}, ...customButtonStyle, style),
            ])
      }>
      <Text onLayout={onLayoutText} style={textStyle ?? customTextStyle}>
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
          }}
        />
      )}
    </Pressable>
  );
};
