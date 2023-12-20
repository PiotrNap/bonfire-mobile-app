import { appContext } from "contexts/contextApi"
import { MnemonicInputsContext } from "contexts/mnemonicInputsContext"
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
  ViewStyle,
} from "react-native"

import { Colors, Sizing } from "styles/index"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"

export type CustomPlainInputProps = TextInputProps & {
  label?: string
  idx?: string | number
  styles?: any
  customInputStyle?: ViewStyle
  customInputWrapperStyle?: ViewStyle
  onPressHandler?: () => void
  icon?: any
  customChild?: React.ReactNode
  maxChar?: number
  isDisabled?: boolean
  labelStyle?: StyleProp<TextStyle>
  onChangeCallback?: (e: any) => void
  onPressInCallback?: (e: any) => void
  onEndEditingCallback?: (e: any, id?: any) => void
  validate?: (val: string, idx: string | number | undefined) => boolean
  onError?: (val: boolean, idx: any) => void
}

export const CustomPlainInput = React.forwardRef((props, ref) => {
  var {
    icon,
    placeholder,
    label,
    idx,
    labelStyle,
    customChild,
    onPressHandler,
    styles,
    customInputStyle,
    customInputWrapperStyle,
    multiline,
    numberOfLines,
    maxChar,
    keyboardType,
    onChangeCallback,
    onEndEditingCallback,
    textContentType,
    defaultValue,
    isDisabled,
    onError,
    validate,
  }: CustomPlainInputProps = props
  const [charsLeft, setCharsLeft] = React.useState<number | null>(null)
  const [isValid, setIsValid] = React.useState<boolean>(true)
  const { addMnemonicInput, focusNextField } = React.useContext(MnemonicInputsContext)
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const inputRef = React.useRef<TextInput>(null)
  const Icon = icon

  React.useImperativeHandle(ref, () => ({
    focus: () => {
      inputRef.current?.focus && inputRef.current.focus()
    },
    idx,
  }))

  React.useEffect(() => {
    addMnemonicInput(idx, inputRef)
  }, [])

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
    styles = Object.assign({}, defaultStyles, styles, inputStyles, formStyleLight)
  } else {
    styles = Object.assign({}, defaultStyles, styles, inputStyles, formStyleDark)
  }
  //@ts-ignore
  const valid = (val) => validate(val, idx)

  const validateInput = (val: string, onBlur: boolean = false) => {
    onError = onError || (() => {})

    if (!validate || !val) {
      return
    } else if (val.length < 3) {
      setIsValid(false)
      onError(false, idx)
    } else {
      setIsValid(valid(val))
      onError(valid(val), idx)
    }
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

  const onChange = (e) =>
    onEndEditingCallback && onEndEditingCallback(e.nativeEvent.text, idx)
  const onEndEditing = (e) =>
    onEndEditingCallback && onEndEditingCallback(e.nativeEvent.text, idx)
  const onSubmitEditing = () => {
    focusNextField(idx)
  }
  const onBlur = (e) => validate && validateInput(e.nativeEvent.text, true)

  return (
    <View style={styles.inputContainer}>
      {label && (
        <View style={styles.labelContainer}>
          <Text style={[styles.label, labelStyle]}>
            {label} {charsLeft && `${charsLeft}/${maxChar}`}
          </Text>
        </View>
      )}
      <View style={[styles.textInputWrapper, customInputWrapperStyle]}>
        <TextInput
          style={[
            styles.input,
            multiline != null && { height: 120, textAlignVertical: "top" },
            isDisabled && { backgroundColor: Colors.neutral.s200 },
            !isValid && styles.errorInput,
            customInputStyle,
          ]}
          ref={inputRef}
          numberOfLines={numberOfLines != null ? numberOfLines : 1}
          placeholder={placeholder}
          onChangeText={onChangeText}
          onChange={onChange}
          onEndEditing={onEndEditing}
          onSubmitEditing={onSubmitEditing}
          placeholderTextColor={styles.placeholderText.color}
          onBlur={onBlur}
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
})

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
