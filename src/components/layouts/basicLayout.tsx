import * as React from "react"
import { StyleSheet } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { appContext } from "contexts/contextApi"
import { Colors } from "styles/index"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"

interface Props {
  children: React.ReactNode
  scrollable?: boolean
}

export const Layout = ({ children, scrollable }: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"

  return !scrollable ? (
    <SafeAreaView
      style={[
        styles.safeArea,
        isLightMode ? styles.safeArea_light : styles.safeaArea_dark,
      ]}>
      {children}
    </SafeAreaView>
  ) : (
    <SafeAreaView
      style={[isLightMode ? styles.safeArea_light : styles.safeaArea_dark]}>
      <KeyboardAwareScrollView
        keyboardShouldPersistTaps="handled"
        showsVerticalScrollIndicator={false}
        keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
        style={{ width: "100%", height: "100%" }}
        contentContainerStyle={{ alignItems: "center" }}>
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
})
