import * as React from "react"
import { Switch, SwitchProps } from "react-native"
import { Colors } from "styles/index"

export interface CustomSwitch extends SwitchProps {}

export const CustomSwitch = ({ onValueChange, value, style }: CustomSwitch) => {
  return (
    <Switch
      trackColor={{
        false: Colors.neutral.s400,
        true: Colors.primary.brand,
      }}
      thumbColor={Colors.primary.neutral}
      ios_backgroundColor={Colors.neutral.s400}
      onValueChange={onValueChange}
      value={value}
      style={style}
    />
  )
}
