import * as React from "react"
import { StyleSheet, View } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { appContext } from "contexts/contextApi"
import { Colors } from "styles/index"

interface Props {
  children: React.ReactNode
}

export const Layout = ({ children }: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"

  return (
    <SafeAreaView
      style={[isLightMode ? styles.safeArea_light : styles.safeaArea_dark]}>
      <View style={{ flex: 1, width: "100%", alignItems: "center" }}>
        {children}
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeaArea_dark: {
    flex: 1,
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
  },
})
