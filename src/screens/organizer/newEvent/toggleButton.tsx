import React, { useState, useRef } from "react"
import {
  View,
  TouchableOpacity,
  Text,
  Animated,
  StyleSheet,
  Dimensions,
} from "react-native"
import { appContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"

// the order of items in options & values arrays should be the same
const _ToggleButton = ({
  options, // to display in UI
  values, // to reference value selected
  defaultValue,
  animationDuration,
  onSelect,
}: {
  options: string[]
  values: any[]
  animationDuration: number
  defaultValue: any
  onSelect: (value: string) => any
}) => {
  const [active, setActive] = useState(options[values.indexOf(defaultValue) || 0])
  const [viewWidth, setViewWidth] = useState(0)
  const animation = useRef(new Animated.Value(values.indexOf(defaultValue) || 0)).current
  const { colorScheme } = appContext()
  const isLigthMode = colorScheme === "light"

  const handlePress = (option: string, value: any) => {
    Animated.timing(animation, {
      toValue: options.indexOf(option),
      duration: animationDuration,
      useNativeDriver: true,
    }).start()
    setActive(option)
    onSelect(value)
  }

  const buttonWidth = Dimensions.get("window").width / options.length

  const translateX = animation.interpolate({
    inputRange: options.map((_, i) => i),
    outputRange: options.map((_, i) => i * (viewWidth / options.length || buttonWidth)),
  })

  if (isLigthMode) {
    styles = Object.assign({}, styles, stylesLight)
  } else {
    styles = Object.assign({}, styles, stylesDark)
  }

  return (
    <View
      style={styles.container}
      onLayout={(e: any) => setViewWidth(e.nativeEvent.layout.width)}>
      <Animated.View
        style={[
          styles.overlay,
          { width: `${100 / options.length}%`, transform: [{ translateX }] },
        ]}
      />
      {options.map((option, index) => (
        <TouchableOpacity
          key={index}
          style={styles.button}
          onPress={() => handlePress(option, values[index])}>
          <Text style={[styles.text, active === option && styles.activeText]}>
            {option}
          </Text>
        </TouchableOpacity>
      ))}
    </View>
  )
}

let styles: any = StyleSheet.create({
  button: {
    flex: 1,
    paddingVertical: Sizing.x8,
    alignItems: "center",
  },
})

const stylesDark = StyleSheet.create({
  container: {
    flexDirection: "row",
    borderColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    overflow: "hidden",
    marginBottom: Sizing.x20,
  },
  text: {
    color: Colors.primary.neutral,
    ...Typography.header.x25,
  },
  activeText: {
    color: Colors.primary.s800,
  },
  overlay: {
    position: "absolute",
    height: "100%",
    backgroundColor: Colors.primary.neutral,
  },
})
const stylesLight = StyleSheet.create({
  container: {
    flexDirection: "row",
    borderColor: Colors.primary.s800,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    overflow: "hidden",
    marginBottom: Sizing.x20,
  },
  text: {
    color: Colors.primary.s800,
    ...Typography.header.x25,
  },
  activeText: {
    color: Colors.primary.neutral,
  },
  overlay: {
    position: "absolute",
    height: "100%",
    backgroundColor: Colors.primary.s800,
  },
})

export const ToggleButton = React.memo(_ToggleButton)
