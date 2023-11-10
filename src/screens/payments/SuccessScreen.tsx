import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { StackScreenProps } from "@react-navigation/stack"

import { Colors, Sizing, Typography } from "styles/index"
import { appContext } from "contexts/contextApi"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { BodyText } from "components/rnWrappers/bodyText"
import { SuccessIcon, LeftArrowIcon } from "assets/icons"
import { AppStackParamList } from "common/types/navigationTypes"

type Props = StackScreenProps<AppStackParamList, "Success">

export const SuccessScreen = ({ navigation, route }: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const { navigationScreen, bodyText, headerText } = route.params
  const navigateBack = () => navigation.navigate(navigationScreen || "Navigation Screens")

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode ? Colors.primary.neutral : Colors.neutral.s600,
        },
      ]}>
      <View style={styles.navigation}>
        <Pressable onPress={navigateBack} hitSlop={10}>
          <LeftArrowIcon
            width={24}
            height={24}
            color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
          />
        </Pressable>
      </View>
      <View style={styles.container}>
        <SuccessIcon width={200} height={200} style={styles.icon} />
        <View style={styles.textContainer}>
          <Text style={isLightMode ? styles.headerText_light : styles.headerText_dark}>
            {headerText ?? "Success!"}
          </Text>
          <BodyText
            customStyle={styles.bodyText}
            changingColorScheme={true}
            colors={[Colors.primary.s600, Colors.primary.neutral]}>
            {bodyText}
          </BodyText>
        </View>
        <View style={styles.buttonContainer}>
          <FullWidthButton
            onPressCallback={navigateBack}
            text={"Close"}
            colorScheme={colorScheme}
          />
        </View>
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
    justifyContent: "flex-start",
    height: "100%",
  },
  container: {
    width: "90%",
    height: "100%",
  },
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  icon: {
    alignSelf: "center",
    marginTop: "auto",
    marginBottom: Sizing.x10,
  },
  textContainer: {
    marginBottom: "auto",
  },
  headerText_light: {
    ...Typography.header.x60,
    color: Colors.primary.s800,
    marginVertical: Sizing.x20,
  },
  headerText_dark: {
    ...Typography.header.x60,
    color: Colors.primary.neutral,
    marginVertical: Sizing.x20,
  },
  bodyText: {},
  buttonContainer: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    marginBottom: Sizing.x80,
    marginTop: "auto",
  },
})
