import * as React from "react"
import { Pressable, StyleSheet, Text, TextInput, View } from "react-native"

import { EyeIcon, EyeOffIcon } from "icons/index"
import { Sizing, Colors } from "styles/index"

export const CustomPasswordInput = (props: any) => {
  const {
    field: { onChange, name, onBlur, value },
    form: { errors, touched, setFieldTouched },
    iconState,
    customHandler,
    submitted,
    validateForm,
    placeholder,
    label,
    textContentType,
    styles,
    showIcon,
    passwordHideCb,
    hidePassword,
    ...inputProps
  } = props

  const PasswordEyeIcon = !hidePassword ? EyeIcon : EyeOffIcon
  const hasError = errors[name] && touched[name]

  return (
    <>
      <View style={styles.labelContainer}>
        <Text style={styles.label}>{label}</Text>
      </View>
      <View style={styles.textInputWrapper}>
        <TextInput
          name={props.name}
          style={[
            defaultStyles.input,
            styles.input,
            hasError && styles.errorInput,
          ]}
          value={value}
          label={label}
          placeholder={placeholder}
          placeholderTextColor={styles.placeholderText.color}
          onChangeText={(text) => onChange(name)(text)}
          onChange={validateForm}
          secureTextEntry={hidePassword}
          textContentType={textContentType}
          onBlur={() => {
            setFieldTouched(name)
            onBlur(name)
          }}
          {...inputProps}
        />
        {showIcon && (
          <Pressable onPress={passwordHideCb} style={defaultStyles.iconWrapper}>
            <PasswordEyeIcon
              stroke={Colors.primary.s800}
              style={defaultStyles.icon}
            />
          </Pressable>
        )}
      </View>
      <View style={styles.errorWrapper}>
        {hasError && <Text style={styles.error}>{errors[name]}</Text>}
      </View>
    </>
  )
}

const defaultStyles = StyleSheet.create({
  input: {
    paddingRight: Sizing.x60,
  },
  iconWrapper: {
    position: "absolute",
    right: 10,
    elevation: 5, // elevation on Android is 4 for text input
    alignItems: "center",
    justifyContent: "center",
    width: Sizing.x40,
    height: Sizing.x40,
  },
  icon: {
    width: Sizing.x30,
    height: Sizing.x30,
  },
})
