import { CheckIcon } from "assets/icons"
import { BodyText } from "components/rnWrappers/bodyText"
import { appContext } from "contexts/contextApi"
import * as React from "react"
import { StyleSheet, Pressable, ViewStyle, View } from "react-native"
import { Colors, Outlines, Sizing } from "styles/index"

export interface Props {
  acceptedCheckbox: boolean
  onCheckBoxPress: (tag?: string) => void
  children?: React.ReactNode
  customStyle?: ViewStyle
  customContainerStyle?: ViewStyle
  colorMode?: "light" | "dark"
  tag?: string
}

export const Checkbox = ({
  acceptedCheckbox,
  onCheckBoxPress,
  customStyle,
  customContainerStyle,
  children,
  colorMode,
  tag,
}: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = (colorMode ?? colorScheme) === "light"
  const onPress = tag ? () => onCheckBoxPress(tag) : onCheckBoxPress
  return (
    <View style={[styles.container, customContainerStyle]}>
      <Pressable
        //@ts-ignore
        onPress={onPress}
        hitSlop={12}
        style={[
          styles.checkbox,
          {
            borderWidth: isLightMode ? Outlines.borderWidth.thin : 0,
            backgroundColor:
              isLightMode && acceptedCheckbox
                ? Colors.primary.s600
                : Colors.primary.neutral,
            borderColor: isLightMode && acceptedCheckbox ? Colors.primary.s600 : "black",
          },
          customStyle,
        ]}>
        <CheckIcon
          width={Sizing.x20}
          height={Sizing.x20}
          strokeWidth="4"
          stroke={
            isLightMode
              ? Colors.primary.neutral
              : !isLightMode && acceptedCheckbox
              ? Colors.primary.s600
              : Colors.primary.neutral
          }
        />
      </Pressable>
      {children && (
        <Pressable
          //@ts-ignore
          onPress={onPress}
          style={{
            width: "90%",
          }}>
          <BodyText
            customStyle={{
              fontFamily: "Roboto-Regular",
              fontSize: Sizing.x14,
            }}
            changingColorScheme
            customColorScheme="dark"
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            {children}
          </BodyText>
        </Pressable>
      )}
    </View>
  )
}

const styles = StyleSheet.create({
  container: { flexDirection: "row", alignItems: "center" },
  checkbox: {
    alignItems: "center",
    justifyContent: "center",
    width: Sizing.x20,
    height: Sizing.x20,
    margin: Sizing.x10,
    borderRadius: Sizing.x3,
  },
})
