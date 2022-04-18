import { appContext } from "contexts/contextApi"
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

import { Colors, Forms, Outlines, Sizing } from "styles/index"

export type CustomPlainInputProps = TextInputProps & {
  label: string
  styles?: any
  onPressHandler?: () => void
  icon?: any
  customChild?: React.ReactNode
  maxChar?: number
  isDisabled?: boolean
  labelStyle?: StyleProp<TextStyle>
  onChangeCallback?: (e: any) => void
  onPressInCallback?: (e: any) => void
  onEndEditingCallback?: (e: any) => void
}

export const CustomPlainInput = ({
  icon,
  placeholder,
  label,
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
}: CustomPlainInputProps) => {
  const [charsLeft, setCharsLeft] = React.useState<number | null>(null)
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
    styles = Object.assign({}, defaultStyles, styles, formStyleLight)
  } else {
    styles = Object.assign({}, defaultStyles, styles, formStyleDark)
  }

  const onChangeText = (val: string) => {
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
      <View style={styles.labelContainer}>
        <Text style={[styles.label, labelStyle]}>
          {label} {charsLeft && `${charsLeft}/${maxChar}`}
        </Text>
      </View>
      <View style={styles.textInputWrapper}>
        <TextInput
          style={[
            styles.input,
            multiline != null && { height: 120, textAlignVertical: "top" },
            isDisabled && { backgroundColor: Colors.neutral.s200 },
          ]}
          numberOfLines={numberOfLines != null ? numberOfLines : 1}
          placeholder={placeholder}
          onChangeText={onChangeText}
          onEndEditing={(e) =>
            onEndEditingCallback && onEndEditingCallback(e.nativeEvent.text)
          }
          placeholderTextColor={styles.placeholderText.color}
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
  inputContainer: {
    width: "100%",
    marginBottom: Sizing.x10,
  },
  labelContainer: {
    width: "100%",
  },
  textInputWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
  },
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

const formStyleLight = StyleSheet.create({
  label: {
    ...Forms.inputLabel.primary_light,
  },
  input: {
    width: "100%",
    ...Forms.input.primary_light,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
})

const formStyleDark = StyleSheet.create({
  label: {
    ...Forms.inputLabel.primary_dark,
  },
  input: {
    width: "100%",
    ...Forms.input.primary_dark,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
})
