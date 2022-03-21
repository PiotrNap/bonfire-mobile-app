import * as React from "react";
import {
  View,
  Text,
  Pressable,
  TextInput,
  StyleSheet,
  TextInputProps,
  KeyboardType,
  TextStyle,
  StyleProp,
} from "react-native";

import { Colors, Forms, Outlines, Sizing } from "styles/index";

export interface CustomPlainInputProps {
  label: string;
  placeholder: string;
  styles?: any;
  isLightMode?: boolean;
  onPressHandler?: () => void;
  icon?: any;
  customChild?: React.ReactNode;
  multiline?: boolean;
  numberOfLines?: number;
  maxChar?: number;
  labelStyle?: StyleProp<TextStyle>;
  keyboardType?: KeyboardType;
  onChangeCallback?: (e: any) => void;
  onPressInCallback?: (e: any) => void;
  onBlurCallback?: (e: any) => void;
}

export const CustomPlainInput = (props: CustomPlainInputProps) => {
  const [charsLeft, setCharsLeft] = React.useState<number | null>(null);
  var {
    icon,
    placeholder,
    label,
    labelStyle,
    customChild,
    onPressHandler,
    styles,
    isLightMode = true,
    multiline,
    numberOfLines,
    maxChar,
    keyboardType,
    onBlurCallback,
    onChangeCallback,
  }: CustomPlainInputProps = props;
  const Icon = icon;

  const additionalProps: TextInputProps = {
    keyboardType: keyboardType ?? "default",
  };

  if (multiline && numberOfLines) {
    additionalProps.multiline = true;
    additionalProps.numberOfLines = numberOfLines;
  }
  if (maxChar) {
    additionalProps.maxLength = maxChar;
  }

  if (isLightMode) {
    styles = Object.assign({}, defaultStyles, styles, formStyleLight);
  } else {
    styles = Object.assign({}, defaultStyles, styles, formStyleDark);
  }

  const onChangeText = (val: string) => {
    if (maxChar && val.length >= maxChar - maxChar / 5) {
      setCharsLeft(val.length);
      onChangeCallback && onChangeCallback(val);
    } else if (charsLeft) {
      setCharsLeft(null);
      onChangeCallback && onChangeCallback(val);
    } else {
      onChangeCallback && onChangeCallback(val);
    }
  };

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
            multiline != null ? { height: 90, textAlignVertical: "top" } : {},
          ]}
          numberOfLines={numberOfLines != null ? numberOfLines : 1}
          placeholder={placeholder}
          onChangeText={onChangeText}
          onBlur={onBlurCallback}
          placeholderTextColor={styles.placeholderText.color}
          {...additionalProps}
        />
        {customChild && customChild}
        <Pressable onPress={onPressHandler} style={styles.iconWrapper}>
          {Icon && <Icon style={styles.icon} stroke={Colors.primary.s350} />}
        </Pressable>
      </View>
    </View>
  );
};

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
});

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
});

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
});
