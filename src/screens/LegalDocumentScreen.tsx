import Markdown from "@valasolutions/react-native-markdown"
import { LeftArrowIcon } from "assets/icons"
import { PrivacyPolicy } from "assets/legal/privacy-policy"
import { TermsOfService } from "assets/legal/terms-of-service"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { appContext } from "contexts/contextApi"
import { useCallback, useEffect, useState } from "react"
import { Pressable, SafeAreaView, StyleSheet, View } from "react-native"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { Colors, Sizing, Typography } from "styles/index"

export const LegalDocumentScreen = ({ navigation, route }: any) => {
  const { colorScheme } = appContext()
  const [content, setContent] = useState<any>(null)
  const [isScrolledToBottom, setIsScrolledToBottom] = useState(false)
  const isLightMode = colorScheme === "light"

  const onScroll = useCallback((event: any) => {
    const { layoutMeasurement, contentOffset, contentSize } = event.nativeEvent
    const isBottom =
      layoutMeasurement.height + contentOffset.y >= contentSize.height * 0.99 // check if screen is at 99% of scrollable content
    setIsScrolledToBottom(isBottom)
  }, [])

  useEffect(() => {
    if (route.params.type === "terms-of-service") {
      setContent(TermsOfService)
    } else if (route.params.type === "privacy-policy") {
      setContent(PrivacyPolicy)
    }
  }, [])

  const navigateBack = () => navigation.goBack()

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode ? Colors.primary.neutral : Colors.neutral.s600,
        },
      ]}>
      <View style={styles.container}>
        <View style={styles.navigation}>
          <Pressable onPress={navigateBack} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>

        <KeyboardAwareScrollView
          onScroll={onScroll}
          style={styles.scrollableView}
          scrollEventThrottle={400} // how long to wait before firing up scroll event
          showsVerticalScrollIndicator={true}
          contentContainerStyle={{ alignItems: "center" }}>
          <Markdown style={markdownStyles}>{content}</Markdown>
        </KeyboardAwareScrollView>
        <View style={styles.buttonContainer}>
          <FullWidthButton
            onPressCallback={navigateBack}
            text={"I accept"}
            colorScheme={colorScheme}
            disabled={!isScrolledToBottom}
          />
        </View>
      </View>
    </SafeAreaView>
  )
}

const markdownStyles = StyleSheet.create({
  heading1: {
    ...Typography.header.x40,
  },
  heading2: {
    ...Typography.header.x35,
  },
  heading3: {
    ...Typography.header.x30,
  },
  body: {
    ...Typography.body.x20,
  },
})

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
    justifyContent: "flex-start",
    height: "100%",
  },
  container: {
    flex: 1,
    width: "90%",
    height: "100%",
  },
  scrollableView: {
    flex: 1,
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
    marginBottom: Sizing.x20,
    marginTop: "auto",
  },
})
