import { appContext } from "contexts/contextApi"
import { getRandomKey } from "lib/utils"
import * as React from "react"
import {
  View,
  Text,
  Pressable,
  TextInput,
  StyleSheet,
  TextInputProps,
  TextStyle,
  StyleProp,
} from "react-native"

import { Colors, Sizing } from "styles/index"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"

export type CustomPlainInputProps = TextInputProps & {
  label?: string
  idx?: string | number
  styles?: any
  onPressHandler?: () => void
  icon?: any
  customChild?: React.ReactNode
  maxChar?: number
  isDisabled?: boolean
  labelStyle?: StyleProp<TextStyle>
  onChangeCallback?: (e: any) => void
  onPressInCallback?: (e: any) => void
  onEndEditingCallback?: (e: any, id?: any) => void
  validate?: (val: string) => boolean
}

export const CustomPlainInput = ({
  icon,
  placeholder,
  label,
  idx,
  labelStyle,
  customChild,
  onPressHandler,
  styles,
  multiline,
  numberOfLines,
  maxChar,
  keyboardType,
  onChangeCallback,
  onEndEditingCallback,
  textContentType,
  defaultValue,
  isDisabled,
  validate,
}: CustomPlainInputProps) => {
  const [charsLeft, setCharsLeft] = React.useState<number | null>(null)
  const [isValid, setIsValid] = React.useState<boolean>(true)
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const Icon = icon

  const additionalProps: TextInputProps = {
    keyboardType: keyboardType ?? "default",
    textContentType: textContentType ?? "none",
  }

  if (multiline && numberOfLines) {
    additionalProps.multiline = true
    additionalProps.numberOfLines = numberOfLines
  }
  if (maxChar) {
    additionalProps.maxLength = maxChar
  }

  if (isLightMode) {
    styles = Object.assign(
      {},
      defaultStyles,
      styles,
      inputStyles,
      formStyleLight
    )
  } else {
    styles = Object.assign(
      {},
      defaultStyles,
      styles,
      inputStyles,
      formStyleDark
    )
  }
  //@ts-ignore
  const valid = (val) => (!validate(val) ? setIsValid(false) : setIsValid(true))

  const validateInput = (val: string, onBlur: boolean = false) => {
    if (!validate) return
    if (!val) return setIsValid(true)
    if (onBlur && val.length < 3) return setIsValid(false)

    if (onBlur && val.length === 3) return valid(val)
    if (val.length > 3) return valid(val)
  }

  const onChangeText = (val: string) => {
    validateInput(val)
    if (maxChar && val.length >= maxChar - maxChar / 5) {
      setCharsLeft(val.length)
      onChangeCallback && onChangeCallback(val)
    } else if (charsLeft) {
      setCharsLeft(null)
      onChangeCallback && onChangeCallback(val)
    } else {
      onChangeCallback && onChangeCallback(val)
    }
  }

  return (
    <View style={styles.inputContainer}>
      {label && (
        <View style={styles.labelContainer}>
          <Text style={[styles.label, labelStyle]}>
            {label} {charsLeft && `${charsLeft}/${maxChar}`}
          </Text>
        </View>
      )}
      <View style={styles.textInputWrapper}>
        <TextInput
          style={[
            styles.input,
            multiline != null && { height: 120, textAlignVertical: "top" },
            isDisabled && { backgroundColor: Colors.neutral.s200 },
            !isValid && styles.errorInput,
          ]}
          numberOfLines={numberOfLines != null ? numberOfLines : 1}
          placeholder={placeholder}
          onChangeText={onChangeText}
          onChange={(e) =>
            onEndEditingCallback &&
            onEndEditingCallback(e.nativeEvent.text, idx)
          }
          onEndEditing={(e) =>
            onEndEditingCallback &&
            onEndEditingCallback(e.nativeEvent.text, idx)
          }
          placeholderTextColor={styles.placeholderText.color}
          onBlur={(e) => validate && validateInput(e.nativeEvent.text, true)}
          defaultValue={defaultValue}
          editable={!isDisabled}
          {...additionalProps}
        />
        {customChild}
        {Icon && (
          <Pressable onPress={onPressHandler} style={styles.iconWrapper}>
            <Icon style={styles.icon} stroke={Colors.primary.s350} />
          </Pressable>
        )}
      </View>
    </View>
  )
}

const defaultStyles = StyleSheet.create({
  iconWrapper: {
    left: -40,
    width: Sizing.x35,
    height: Sizing.x35,
    alignItems: "center",
    justifyContent: "center",
  },
  icon: {
    width: Sizing.x30,
    height: Sizing.x30,
  },
})
