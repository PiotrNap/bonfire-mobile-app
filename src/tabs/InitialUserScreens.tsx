import * as React from "react"
import { View, Animated, StyleSheet, Dimensions, Pressable } from "react-native"

import {
  UserDetailsScreen,
  RegistrationConfirmationScreen,
} from "screens/onboarding/index"
import { SafeAreaView } from "react-native-safe-area-context"
import PagerView from "react-native-pager-view"
import { ScalingDot } from "react-native-animated-pagination-dots"
import { Colors, Outlines, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import { MnemonicPreview } from "screens/wallet/MnemonicPreviewScreen"
import { MnemonicInsertScreen } from "screens/wallet/MnemonicInsertScreen"
import { LeftArrowIcon } from "assets/icons"
import { NewWalletSetUp } from "screens/wallet/NewWalletSetUpScreen"
import { ImportMnemonicConfirmation } from "screens/wallet/ImportMnemonicConfirmationScreen"

const AnimatedPagerView = Animated.createAnimatedComponent(PagerView)

const CREATE_ACCOUNT_SCREENS = [
  { component: UserDetailsScreen },
  { component: MnemonicPreview },
  { component: MnemonicInsertScreen },
  { component: NewWalletSetUp },
  { component: RegistrationConfirmationScreen },
]

const SIGN_IN_SCREENS = [
  { component: MnemonicInsertScreen, prop: "sign-in" },
  { component: ImportMnemonicConfirmation, prop: "sign-in" },
  { component: NewWalletSetUp, prop: "sign-in" },
]

export const InitialUserScreens = ({ route }: any) => {
  const { params } = route || {}
  let SCREENS =
    params === "create-account" ? CREATE_ACCOUNT_SCREENS : SIGN_IN_SCREENS
  const { pageIndex, setRef, ref: _ref } = appContext()
  const ref = React.useRef<PagerView>(null)
  const screenWidth = Dimensions.get("window").width
  const scrollOffsetAnimatedValue = React.useRef(new Animated.Value(0)).current
  const positionAnimatedValue = React.useRef(new Animated.Value(0)).current
  const inputRange = [0, SCREENS.length]
  const scrollX = Animated.add(
    scrollOffsetAnimatedValue,
    positionAnimatedValue
  ).interpolate({
    inputRange,
    outputRange: [0, SCREENS.length * screenWidth],
  })

  React.useEffect(() => {
    // set ref of View Pager so that we can manipulate page index
    if (ref) {
      setRef(ref)
    }
  }, [])

  // This is only working with useMemo/useCallback
  const onPageScroll = React.useMemo(
    () =>
      Animated.event(
        [
          {
            nativeEvent: {
              offset: scrollOffsetAnimatedValue,
              position: positionAnimatedValue,
            },
          },
        ],
        { useNativeDriver: false }
      ),
    []
  )

  const renderScreens = ({ component, prop }: any, i: number) => {
    const ScreenComponent = component
    return (
      <View style={styles.pagerViewItem} key={i}>
        <ScreenComponent pageIndex={i} pagerRef={ref} prop={prop} />
      </View>
    )
  }

  return (
    <SafeAreaView style={styles.safeArea}>
      <AnimatedPagerView
        ref={ref}
        keyboardDismissMode="on-drag"
        showPageIndicator={false}
        onPageScroll={onPageScroll}
        scrollEnabled={false}
        initialPage={pageIndex}
        style={styles.animatedPager}>
        {SCREENS.map(renderScreens)}
      </AnimatedPagerView>
      <View style={styles.dotContainer}>
        {/* @ts-ignore */}
        {ref.current?.state?.page > 0 && (
          <View style={styles.navigation}>
            <Pressable
              /* @ts-ignore */
              onPress={() => ref.current?.setPage(ref.current.state.page - 1)}
              hitSlop={10}>
              <LeftArrowIcon
                width={24}
                height={24}
                color={Colors.primary.neutral}
              />
            </Pressable>
          </View>
        )}
        <ScalingDot
          data={SCREENS}
          //@ts-ignore
          scrollX={scrollX}
          inActiveDotColor={Colors.transparent.clear}
          activeDotScale={0.95}
          activeDotColor={Colors.primary.neutral}
          dotStyle={styles.dot}
        />
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    backgroundColor: Colors.primary.s800,
    marginHorizontal: "auto",
  },
  animatedPager: {
    // so that dots doesn't overlay the content
    flex: 12,
  },
  pagerViewItem: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
  },
  dotContainer: {
    flex: 1,
  },
  dot: {
    borderRadius: Outlines.borderRadius.max,
    borderWidth: Outlines.borderWidth.base,
    borderColor: Colors.primary.brand,
    padding: 8,
  },
  navigation: {
    marginTop: Sizing.x15,
    width: "90%",
  },
})
