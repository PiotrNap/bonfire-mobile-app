import * as React from "react"
import { Pressable, StyleSheet, View } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { appContext } from "contexts/contextApi"
import { Colors, Sizing } from "styles/index"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { LeftArrowIcon } from "assets/icons"

interface Props {
  children: React.ReactNode
  scrollable?: boolean
  additionalScrolling?: boolean
  backNavigationIcon?: boolean
  backNavigationCb?: () => void
}

export const Layout = ({
  children,
  scrollable,
  additionalScrolling,
  backNavigationIcon,
  backNavigationCb,
}: Props) => {
  const { colorScheme } = appContext()
  const [scrollY, setScrollY] = React.useState(0)
  const isLightMode = colorScheme === "light"
  const scrollRef = React.useRef(null)

  const handleScroll = (event) => {
    const currentOffsetY = event.nativeEvent.contentOffset.y
    setScrollY(currentOffsetY) // Update scroll position
  }
  const scrollBy = (offsetY) => {
    // Add an offset to the current scroll position and scroll to the new position
    if (scrollRef.current) {
      scrollRef.current.scrollToPosition(0, scrollY + offsetY, true)
    }
  }
  return !scrollable ? (
    <SafeAreaView
      style={[
        styles.safeArea,
        isLightMode ? styles.safeArea_light : styles.safeaArea_dark,
      ]}>
      {backNavigationIcon && (
        <View style={styles.navigation}>
          <Pressable onPress={backNavigationCb} hitSlop={10}>
            <LeftArrowIcon
              strokeWidth={Sizing.x3}
              width={Sizing.x25}
              height={Sizing.x25}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
      )}
      {children}
    </SafeAreaView>
  ) : (
    <SafeAreaView style={[isLightMode ? styles.safeArea_light : styles.safeaArea_dark]}>
      <KeyboardAwareScrollView
        ref={scrollRef}
        scrollEventThrottle={400} // how long to wait before firing up scroll event
        keyboardShouldPersistTaps="handled"
        showsVerticalScrollIndicator={true}
        keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
        scrollToOverflowEnabled={true}
        scrollEnabled={true}
        onScroll={handleScroll}
        style={{ width: "100%", height: "100%" }}
        onKeyboardWillShow={(frames: Object) => {
          if (additionalScrolling) scrollBy(120)
        }}
        automaticallyAdjustsScrollIndicatorInsets
        contentContainerStyle={{ alignItems: "center" }}>
        {backNavigationIcon && (
          <View style={styles.navigation}>
            <Pressable onPress={backNavigationCb} hitSlop={10}>
              <LeftArrowIcon
                strokeWidth={Sizing.x3}
                width={Sizing.x25}
                height={Sizing.x25}
                color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
              />
            </Pressable>
          </View>
        )}
        {children}
      </KeyboardAwareScrollView>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    width: "100%",
    alignItems: "center",
  },
  safeArea_light: {
    backgroundColor: Colors.primary.neutral,
  },
  safeaArea_dark: {
    backgroundColor: Colors.neutral.s600,
  },
  navigation: {
    marginTop: Sizing.x15,
    width: "90%",
  },
})
