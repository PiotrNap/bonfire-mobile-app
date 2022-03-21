import * as React from "react";
import { View, Animated, StyleSheet, Dimensions } from "react-native";

import {
  CreateAccountScreen,
  InitialScreen,
  PricingScreen,
} from "screens/onboarding";
import PagerView from "react-native-pager-view";
import { ScalingDot } from "react-native-animated-pagination-dots";
import { Colors, Outlines } from "styles/index";
import { SafeAreaView } from "react-native-safe-area-context";

const AnimatedPagerView = Animated.createAnimatedComponent(PagerView);

const SCREENS = [
  { component: InitialScreen },
  { component: PricingScreen },
  { component: CreateAccountScreen },
];

export const OnboardingScreens = () => {
  const ref = React.useRef<PagerView>(null);
  const screenWidth = Dimensions.get("window").width;
  const scrollOffsetAnimatedValue = React.useRef(new Animated.Value(0)).current;
  const positionAnimatedValue = React.useRef(new Animated.Value(0)).current;
  const inputRange = [0, SCREENS.length];
  const scrollX = Animated.add(
    scrollOffsetAnimatedValue,
    positionAnimatedValue
  ).interpolate({
    inputRange,
    outputRange: [0, SCREENS.length * screenWidth],
  });

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
  );

  const renderScreens = ({ component }: any, i: number) => {
    const ScreenComponent = component;
    return (
      <View style={styles.pagerViewItem} key={i}>
        <ScreenComponent pagerRef={ref} />
      </View>
    );
  };

  return (
    <SafeAreaView style={styles.safeArea}>
      <AnimatedPagerView
        ref={ref}
        onPageScroll={onPageScroll}
        keyboardDismissMode="on-drag"
        initialPage={0}
        scrollEnabled={false}
        style={styles.animatedPager}>
        {SCREENS.map(renderScreens)}
      </AnimatedPagerView>
      <View style={styles.dotContainer}>
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
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    backgroundColor: Colors.primary.s600,
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
});
