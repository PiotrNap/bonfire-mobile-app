import { LeftArrowIcon } from "assets/icons"
import { appContext } from "contexts/contextApi"
import { Pressable, SafeAreaView, StyleSheet, View } from "react-native"
import { Colors, Sizing } from "styles/*"

export function PreviewTransactionScreen({ navigation, route }: any) {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const onBackNavigationPress = () => navigation.goBack()

  return (
    <SafeAreaView style={[isLightMode ? styles.safeArea_light : styles.safeArea_dark]}>
      <View style={styles.mainContainer}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
    flex: 1,
  },
  safeArea_dark: {
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
    flex: 1,
  },
  mainContainer: {
    flex: 1,
    width: "90%",
    marginBottom: Sizing.x5,
  },
  header: {
    width: "100%",
    marginBottom: Sizing.x5,
  },
  navigation: {
    flexDirection: "row",
    marginVertical: Sizing.x15,
  },
})
