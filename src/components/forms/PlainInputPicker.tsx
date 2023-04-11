import * as React from "react"
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  Platform,
  LayoutChangeEvent,
  LayoutRectangle,
  Animated,
} from "react-native"

import { Colors, Outlines, Sizing } from "styles/index"
import { Picker } from "@react-native-picker/picker"
import { DownIcon } from "assets/icons"
import { appContext } from "contexts/contextApi"
import { formStyleDark, formStyleLight } from "../../styles/forms"

export interface PlainInputPickerProps {
  label: string
  minTime?: number
  maxTime?: number
  inputRange: any[]
  styles?: any
  enabledPicker: boolean
  openPicker: string | null
  onValueChange: (val: number) => void
  onOpenChange: (arg: string | null) => void
}

export const PlainInputPicker = (props: PlainInputPickerProps) => {
  const { colorScheme } = appContext()
  const [, setIconAnimationValue] = React.useState<number>(0)
  const [dropDownAnimationValue, setDropDownAnimationValue] =
    React.useState<number>(0)
  const [dimensions, setDimensions] = React.useState<LayoutRectangle | null>(
    null
  )
  const [showPicker, setShowPicker] = React.useState<boolean>(false)
  const [inputValue, setInputValue] = React.useState<any>(null)
  const isLightMode = colorScheme === "light"
  var {
    label,
    inputRange,
    styles,
    minTime,
    maxTime,
    openPicker,
    enabledPicker,
    onValueChange,
    onOpenChange,
  }: PlainInputPickerProps = props
  const iconRotationRef = React.useRef(new Animated.Value(0)).current
  const dropDownHeightRef = React.useRef(new Animated.Value(0)).current
  const isDisabled = inputValue === "--"
  const os = Platform.OS

  React.useEffect(() => {
    setInputValue(minTime || maxTime || inputRange[0] || "--")
  }, [inputRange, minTime, maxTime])

  if (isLightMode) {
    styles = Object.assign({}, defaultStyles, styles, formStyleLight)
  } else {
    styles = Object.assign({}, defaultStyles, styles, formStyleDark)
  }

  React.useEffect(() => {
    // whenever user opens another drop down, close the current open one
    if (!openPicker && showPicker) onInputPress()

    const listeners = () => {
      iconRotationRef.addListener(({ value }) => {
        setIconAnimationValue(value)
      })
      dropDownHeightRef.addListener(({ value }) =>
        setDropDownAnimationValue(value)
      )
    }
    listeners()

    const removeListeners = () => {
      iconRotationRef.removeAllListeners
      dropDownHeightRef.removeAllListeners
    }

    return removeListeners
  }, [openPicker])

  const AnimatedIcon = Animated.createAnimatedComponent(DownIcon)

  const spin = iconRotationRef.interpolate({
    inputRange: [0, 1],
    outputRange: ["0deg", "180deg"],
  })

  const onLayout = (e: LayoutChangeEvent) => setDimensions(e.nativeEvent.layout)

  const onChange = (val: string) => {
    onValueChange(Number(val))
  }

  /**
   * Animations
   */
  const iconAnimation = Animated.timing(iconRotationRef, {
    toValue: (iconRotationRef as any)._value === 0 ? 1 : 0,
    duration: 120,
    useNativeDriver: true,
  })
  const dropDownAnimation = Animated.timing(dropDownHeightRef, {
    toValue: dropDownAnimationValue === 0 ? 150 : 0,
    duration: 120,
    useNativeDriver: true,
  })

  const onInputPress = () => {
    if (openPicker && openPicker !== label) return onOpenChange(null)
    const animations: any[] = []
    animations.push(iconAnimation)
    if (os === "ios") {
      animations.push(dropDownAnimation)
    }
    // on Android we can close by pressing buttons, we do not need
    // change showTimePicker value
    setShowPicker((prev: boolean) => {
      if (os === "android" && prev) {
        return false
      }
      return !prev
    })
    // update the reference of current open picker
    if (!openPicker && !showPicker) {
      onOpenChange(label)
    } else {
      onOpenChange(null)
    }
    Animated.parallel(animations).start()
  }

  const PickerItem = (val: number) => (
    <Picker.Item key={val} label={String(val)} value={String(val)} />
  )

  const renderPickerItems = React.useCallback(
    () => inputRange.map((val) => PickerItem(val)),
    [inputRange]
  )

  return (
    <View style={styles.inputContainer}>
      <View style={styles.labelContainer}>
        <Text style={styles.label}>{label}</Text>
      </View>
      <Pressable
        onPress={onInputPress}
        disabled={isDisabled}
        onLayout={onLayout}
        style={styles.input}>
        <View style={styles.textInputWrapper}>
          <Text
            style={[styles.placeholderText, os === "ios" && { lineHeight: 0 }]}>
            {inputValue + " min"}
          </Text>
          <AnimatedIcon
            style={[styles.icon, { transform: [{ rotate: spin }] }]}
            stroke={Colors.primary.s350}
          />
        </View>
        {os === "ios" && showPicker && (
          <Animated.View
            style={[
              styles.iosPicker,
              dimensions && {
                top: dimensions.height,
                width: dimensions.width,
              },
              { height: dropDownAnimationValue },
            ]}>
            <Picker
              enabled={enabledPicker}
              selectedValue={String(minTime ?? maxTime)}
              onValueChange={onChange}>
              {renderPickerItems()}
            </Picker>
          </Animated.View>
        )}
        {os === "android" && (
          <Picker
            testID={label}
            nativeID={label}
            //@ts-ignore
            onFocus={({ _targetInst }) =>
              _targetInst.pendingProps?.testID === label &&
              iconAnimation.start()
            }
            //@ts-ignore
            onBlur={({ _targetInst }) =>
              _targetInst.pendingProps?.testID === label &&
              iconAnimation.start()
            }
            enabled={enabledPicker}
            style={[
              styles.androidPicker,
              dimensions && {
                width: dimensions.width,
                height: dimensions.height,
              },
            ]}
            selectedValue={String(minTime ?? maxTime)}
            onValueChange={onChange}>
            {renderPickerItems()}
          </Picker>
        )}
      </Pressable>
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
    width: Sizing.x30,
    height: Sizing.x30,
    marginLeft: "auto",
    alignItems: "center",
    justifyContent: "center",
  },
  iosPicker: {
    position: "absolute",
    overflow: "hidden",
    justifyContent: "center",
    backgroundColor: Colors.neutral.s150,
    borderRadius: Outlines.borderRadius.base,
  },
  androidPicker: {
    opacity: 0,
    position: "absolute",
    top: 0,
    left: 0,
  },
  icon: {
    width: Sizing.x30,
    height: Sizing.x30,
    position: "absolute",
    right: 0,
  },
})
