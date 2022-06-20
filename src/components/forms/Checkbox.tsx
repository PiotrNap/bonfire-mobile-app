import { CheckIcon } from "assets/icons"
import { BodyText } from "components/rnWrappers/bodyText"
import { appContext } from "contexts/contextApi"
import * as React from "react"
import { StyleSheet, Pressable, ViewStyle } from "react-native"
import { Colors, Outlines, Sizing } from "styles/index"

export interface Props {
  acceptedCheckbox: boolean
  onCheckBoxPress: () => void
  children: React.ReactNode
  customStyle?: ViewStyle
}

export const Checkbox = ({
  acceptedCheckbox,
  onCheckBoxPress,
  customStyle,
  children,
}: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  return (
    <>
      <Pressable
        onPress={onCheckBoxPress}
        hitSlop={10}
        style={[
          styles.checkbox,
          {
            borderWidth: isLightMode ? Outlines.borderWidth.thin : 0,
            backgroundColor:
              isLightMode && acceptedCheckbox
                ? Colors.primary.s600
                : Colors.primary.neutral,
            borderColor:
              isLightMode && acceptedCheckbox ? Colors.primary.s600 : "black",
          },
          customStyle,
        ]}>
        <CheckIcon
          width="15"
          height="15"
          strokeWidth="3.5"
          stroke={
            isLightMode
              ? Colors.primary.neutral
              : !isLightMode && acceptedCheckbox
              ? Colors.primary.s600
              : Colors.primary.neutral
          }
        />
      </Pressable>
      <BodyText
        customStyle={{
          fontFamily: "Roboto-Regular",
          fontSize: Sizing.x14,
          width: "90%",
        }}
        changingColorScheme
        colors={[Colors.primary.s800, Colors.primary.neutral]}>
        {children}
      </BodyText>
    </>
  )
}

const styles = StyleSheet.create({
  checkbox: {
    alignItems: "center",
    justifyContent: "center",
    width: 17,
    height: 17,
    marginTop: Sizing.x5,
    marginRight: Sizing.x10,
    marginLeft: Sizing.x2,
    borderRadius: Sizing.x3,
  },
})
