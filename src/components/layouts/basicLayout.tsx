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
  backNavigationIcon?: boolean
  backNavigationCb?: () => void
}

export const Layout = ({
  children,
  scrollable,
  backNavigationIcon,
  backNavigationCb,
}: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  // const [isScrolledToBottom, setIsScrolledToBottom] = React.useState(false)

  // const onScroll = (event: any) => {
  //   const { layoutMeasurement, contentOffset, contentSize } = event.nativeEvent
  //   const isBottom =
  //     layoutMeasurement.height + contentOffset.y >= contentSize.height
  //   setIsScrolledToBottom(isBottom)
  // }

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
              width={24}
              height={24}
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
        scrollEventThrottle={400} // how long to wait before firing up scroll event
        keyboardShouldPersistTaps="handled"
        showsVerticalScrollIndicator={true}
        keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
        style={{ width: "100%", height: "100%" }}
        contentContainerStyle={{ alignItems: "center" }}>
        {backNavigationIcon && (
          <View style={styles.navigation}>
            <Pressable onPress={backNavigationCb} hitSlop={10}>
              <LeftArrowIcon
                width={24}
                height={24}
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
