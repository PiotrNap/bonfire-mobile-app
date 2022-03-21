import * as React from "react";
import {
  View,
  Text,
  Pressable,
  StyleSheet,
  Animated,
  LayoutChangeEvent,
  LayoutRectangle,
  Platform,
} from "react-native";

import DateTimePicker from "@react-native-community/datetimepicker";
import { Colors, Forms, Outlines, Sizing, Typography } from "styles/index";
import { DownIcon } from "assets/icons";
import { getDigitalTime } from "lib/utils";

export interface TimePickerInputProps {
  label: string;
  styles?: any;
  timeValue: Date;
  isLightMode?: boolean;
  openPicker: string | null;
  onValueChange: (arg1: string, arg2: any) => void;
  onOpenChange: (arg: string | null) => void;
}

export const TimePickerInput = (props: TimePickerInputProps) => {
  const [showTimePicker, setShowTimePicker] = React.useState<boolean>(false);
  const [iconAnimationValue, setIconAnimationValue] = React.useState<number>(0);
  const [dropDownAnimationValue, setDropDownAnimationValue] =
    React.useState<number>(0);
  const [dimensions, setDimensions] = React.useState<LayoutRectangle | null>(
    null
  );
  var {
    label,
    styles,
    isLightMode = true,
    timeValue,
    onValueChange,
    onOpenChange,
    openPicker,
  }: TimePickerInputProps = props;
  const iconRotationRef = React.useRef(new Animated.Value(0)).current;
  const dropDownHeightRef = React.useRef(new Animated.Value(0)).current;
  const os = Platform.OS;

  if (isLightMode) {
    styles = Object.assign({}, defaultStyles, styles, formStyleLight);
  } else {
    styles = Object.assign({}, defaultStyles, styles, formStyleDark);
  }

  React.useEffect(() => {
    // whenever user opens another drop down, close the current open one
    if (!openPicker && showTimePicker) onInputPress();

    const listeners = () => {
      iconRotationRef.addListener(({ value }) => {
        setIconAnimationValue(value);
      });
      dropDownHeightRef.addListener(({ value }) =>
        setDropDownAnimationValue(value)
      );
    };
    listeners();

    const removeListeners = () => {
      iconRotationRef.removeAllListeners;
      dropDownHeightRef.removeAllListeners;
    };

    return removeListeners;
  }, [openPicker]);

  const AnimatedIcon = Animated.createAnimatedComponent(DownIcon);

  const spin = iconRotationRef.interpolate({
    inputRange: [0, 1],
    outputRange: ["0deg", "180deg"],
  });

  const onInputPress = (e?: any, selectedDate?: any) => {
    if (openPicker && openPicker !== label) return onOpenChange(null);

    // Whenever there is a selected value event from android component
    // just turn the icon
    const iconAnimation = Animated.timing(iconRotationRef, {
      toValue: (iconRotationRef as any)._value === 0 ? 1 : 0,
      duration: 120,
      useNativeDriver: true,
    });
    const dropDownAnimation = Animated.timing(dropDownHeightRef, {
      toValue: dropDownAnimationValue === 0 ? 150 : 0,
      duration: 120,
      useNativeDriver: true,
    });

    const animations: any[] = [];
    animations.push(iconAnimation);
    if (os === "ios") {
      animations.push(dropDownAnimation);
    }

    // on Android we can close by pressing buttons, we do not need
    // change showTimePicker value
    setShowTimePicker((prev: boolean) => {
      if (os === "android" && prev) {
        return false;
      }
      return !prev;
    });
    // update the reference of current open picker
    if (!openPicker && !showTimePicker) {
      onOpenChange(label);
    } else {
      onOpenChange(null);
    }

    Animated.parallel(animations).start(({ finished }) => {
      if (finished) {
        selectedDate && onValueChange(label, selectedDate);
      }
    });
  };

  const onLayout = (e: LayoutChangeEvent) => {
    setDimensions(e.nativeEvent.layout);
  };

  const onChange = (e: any, selectedDate: any) => {
    if (os === "android") {
      onInputPress(e.type, selectedDate);
    } else {
      selectedDate && onValueChange(label, selectedDate);
    }
  };

  return (
    <View style={styles.inputContainer}>
      <View style={styles.labelContainer}>
        <Text style={styles.label}>{label}</Text>
      </View>
      <Pressable
        onLayout={onLayout}
        onPress={onInputPress}
        style={styles.input}>
        <View style={styles.textInputWrapper}>
          <Text
            style={[styles.placeholderText, os === "ios" && { lineHeight: 0 }]}>
            {getDigitalTime(timeValue, "12")}
          </Text>
          <AnimatedIcon
            style={[styles.icon, { transform: [{ rotate: spin }] }]}
            stroke={Colors.primary.s350}
          />
        </View>
        {os === "ios" && (
          <Animated.View
            style={[
              styles.dropDown,
              dimensions && {
                top: dimensions.height,
                width: dimensions.width,
              },
              { height: dropDownAnimationValue },
            ]}>
            <DateTimePicker
              style={styles.dateTimePicker}
              value={timeValue}
              mode="time"
              display="spinner"
              onChange={onChange}
              minuteInterval={5}
            />
          </Animated.View>
        )}
      </Pressable>
      {os === "android" && showTimePicker && (
        <DateTimePicker
          style={styles.dateTimePicker}
          value={timeValue}
          mode="time"
          display="spinner"
          onChange={onChange}
          minuteInterval={5}
        />
      )}
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
    justifyContent: "center",
  },
  icon: {
    width: Sizing.x30,
    height: Sizing.x30,
    position: "absolute",
    right: 0,
  },
  dropDown: {
    height: 0,
    position: "absolute",
    top: 0,
    overflow: "hidden",
    justifyContent: "center",
    backgroundColor: Colors.neutral.s150,
    borderRadius: Outlines.borderRadius.base,
  },
  dateTimePicker: {},
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
    color: Colors.primary.s600,
    ...Typography.subHeader.x30,
    marginRight: "auto",
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
    color: Colors.primary.s600,
    ...Typography.subHeader.x30,
    marginRight: "auto",
  },
});
