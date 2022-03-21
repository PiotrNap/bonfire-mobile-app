import * as React from "react";
import { Pressable, StyleSheet, Text, TextInput, View } from "react-native";

import { EyeIcon, EyeOffIcon } from "icons/index";
import { Sizing, Forms, Colors, Outlines } from "styles/index";

export const CustomPasswordInput = (props: any) => {
  const [isVisiblePassword, setIsVisiblePassword] =
    React.useState<boolean>(false);

  const {
    field: { onChange, name, onBlur, value },
    form: { errors, touched, setFieldTouched },
    iconState,
    customHandler,
    submitted,
    validateForm,
    ...inputProps
  } = props;

  const onEyeIconPress = () => {
    setIsVisiblePassword((prev) => !prev);
  };
  const PasswordEyeIcon = isVisiblePassword ? EyeIcon : EyeOffIcon;

  const hasError = errors[name] && touched[name];

  //@TODO: Fix form validation

  return (
    <View style={styles.inputContainer}>
      <View style={styles.labelContainer}>
        <Text style={styles.label}>Password</Text>
      </View>
      <View style={styles.textInputWrapper}>
        <TextInput
          name={name}
          style={[styles.input, hasError && styles.errorInput]}
          value={value}
          label="Password"
          placeholder="Enter your password"
          placeholderTextColor={styles.placeholderText.color}
          onChangeText={(text) => onChange(name)(text)}
          onEndEditing={() => validateForm()}
          onChange={() => validateForm()}
          secureTextEntry={isVisiblePassword}
          textContentType="newPassword"
          onBlur={() => {
            setFieldTouched(name);
            onBlur(name);
          }}
          {...inputProps}
        />
        <Pressable onPress={onEyeIconPress} style={styles.iconWrapper}>
          <PasswordEyeIcon stroke={Colors.primary.s350} style={styles.icon} />
        </Pressable>
      </View>
      <View
        style={[
          styles.errorWrapper,
          {
            backgroundColor: !hasError ? "transparent" : Colors.danger.s300,
          },
        ]}>
        {hasError && <Text style={styles.error}>{errors[name]}</Text>}
      </View>
    </View>
  );
};

const styles = StyleSheet.create({
  inputContainer: {
    width: "100%",
    alignItems: "center",
  },
  labelContainer: {
    width: "100%",
  },
  label: {
    ...Forms.inputLabel.primary,
  },
  textInputWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
  },
  input: {
    width: "100%",
    ...Forms.input.primary,
    borderColor: "transparent",
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
  iconWrapper: {
    left: -40,
    alignItems: "center",
    justifyContent: "center",
  },
  icon: {
    width: Sizing.x30,
    height: Sizing.x30,
  },
  errorInput: {
    borderColor: Colors.danger.s300,
  },
  errorWrapper: {
    height: 22, // inspect element in expo to see how much pixels it needs
    alignItems: "center",
    paddingHorizontal: Sizing.x8,
    marginTop: Sizing.x5,
    justifyContent: "center",
    borderRadius: Outlines.borderRadius.base,
  },
  error: {
    ...Forms.inputLabel.error,
    color: Colors.primary.neutral,
  },
});
