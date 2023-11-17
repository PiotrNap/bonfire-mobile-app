import * as React from "react"
import { TextInput, View } from "react-native"
import { Colors, Sizing } from "styles/index"

export const TokenInput = ({
  onChange,
  onBlur,
  value,
  name,
  index,
  styles,
  errors,
  isDisabled,
  ...props
}: any) => {
  const key = name.split(".")[1]
  const hasError = errors.paymentTokens?.[index]?.[key]

  return (
    <View style={[styles?.textInputWrapper, { flex: 1, marginHorizontal: Sizing.x2 }]}>
      <TextInput
        style={[
          styles?.input,
          hasError && styles.errorInput,
          isDisabled && {
            backgroundColor: Colors.neutral.s200,
          },
        ]}
        keyboardType={props?.keyboardType || "default"}
        editable={!isDisabled}
        value={String(value)}
        onChangeText={onChange}
        onBlur={onBlur}
        {...props}
      />
    </View>
  )
}
