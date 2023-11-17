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

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { Picker } from "@react-native-picker/picker"
import { DownIcon } from "assets/icons"
import { appContext } from "contexts/contextApi"
import { formStyleDark, formStyleLight } from "../../styles/forms"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"

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
      dropDownHeightRef.addListener(({ value }) =>
        setDropDownAnimationValue(value)
      )
    }
    listeners()

    const removeListeners = () => {
      dropDownHeightRef.removeAllListeners
    }

    return removeListeners
  }, [openPicker])

  const onLayout = (e: LayoutChangeEvent) => setDimensions(e.nativeEvent.layout)
  const onChange = (val: string) => {
    onValueChange(Number(val))
  }

  const dropDownAnimation = Animated.timing(dropDownHeightRef, {
    toValue: dropDownAnimationValue === 0 ? 150 : 0,
    duration: 120,
    useNativeDriver: true,
  })

  const onInputPress = () => {
    if (openPicker && openPicker !== label) return onOpenChange(null)
    const animations: any[] = []
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
      <SubHeaderText
        customStyle={defaultStyles.label}
        colors={[Colors.primary.s800, Colors.primary.neutral]}>
        {label}
      </SubHeaderText>
      <Pressable
        onPress={onInputPress}
        disabled={isDisabled}
        onLayout={onLayout}
        style={styles.input}>
        <View style={styles.textInputWrapper}>
          <Text
            style={[
              { color: Colors.primary.s600 },
              os === "ios" && { lineHeight: 0 },
            ]}>
            {inputValue + " min"}
          </Text>
          <DownIcon
            style={styles.icon}
            stroke={Colors.primary.s600}
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
  label: {
    ...Typography.subHeader.x10,
    paddingLeft: Sizing.x5,
    paddingBottom: Sizing.x5,
  },
})
