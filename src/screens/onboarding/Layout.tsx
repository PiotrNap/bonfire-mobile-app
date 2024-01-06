import { View, StyleSheet } from "react-native"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { Colors, Sizing, Typography } from "styles/index"

export const Layout = ({ children, scrollable }) => {
  return scrollable ? (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={false}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={styles.container}>
      {children}
    </KeyboardAwareScrollView>
  ) : (
    <View style={[styles.container, { justifyContent: "space-between", flex: 1 }]}>
      {children}
    </View>
  )
}

const styles = StyleSheet.create({
  container: { width: "90%", marginVertical: Sizing.x15 },
})
