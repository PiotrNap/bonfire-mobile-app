/**
 *  @description This is a custom form input which can be customized
 *               by passing props (label, styles, name, etc.)
 *
 *               This file doesn't hold any styles declared, as to
 *               provide flexibility on passing custom styles.
 *
 *               This component works with Formik.
 */
import * as React from "react"
import { TextInput, Text, View } from "react-native"
import { Colors } from "styles/index"

export const CustomInput = (props: any) => {
  let {
    field: { onChange, name, onBlur, value },
    form: { errors, touched, setFieldTouched },
    styles,
    validateForm,
    defaultValue,
    placeholder,
    customOnChange,
    isDisabled,
    ...inputProps
  } = props
  const hasError = errors[name] && touched[name]
  const isNumeric = props.field.name === "ada"

  return (
    <>
      {props.label && (
        <View style={styles.labelContainer}>
          <Text style={styles.label}>{props.label}</Text>
        </View>
      )}
      <View style={styles.textInputWrapper}>
        <TextInput
          name={props.name}
          style={[
            styles.input,
            hasError && styles.errorInput,
            inputProps.multiline && {
              height: 100,
              textAlignVertical: "top",
            },
            isDisabled && {
              backgroundColor: Colors.neutral.s200,
            },
          ]}
          editable={!isDisabled}
          value={String(value ?? (isNumeric ? defaultValue ?? 0 : ""))}
          placeholderTextColor={styles.placeholderText.color}
          placeholder={placeholder}
          onEndEditing={() => validateForm()}
          onChangeText={(text) => {
            onChange(name)(text)
            customOnChange && customOnChange(text)
          }}
          onBlur={() => {
            setFieldTouched(name)
            onBlur(name)
          }}
          onSubmitEditing={() => {
            setFieldTouched(name)
            onBlur(name)
          }}
          {...inputProps}
        />
      </View>
      <View style={styles.errorWrapper}>
        {hasError && <Text style={styles.error}>{errors[name]}</Text>}
      </View>
    </>
  )
}
