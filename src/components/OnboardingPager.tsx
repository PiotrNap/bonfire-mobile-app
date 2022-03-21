import * as React from "react";

import { Pressable, StyleSheet } from "react-native";
import PagerView from "react-native-pager-view";
import Animated from "react-native-reanimated";

export const OnboardingScreens = ({
  descriptors,
  navigation,
  state,
  position,
}: any) => {
  const renderScreens = (route: any, index: number) => {
    // no need to show label for page swiper

    const { options } = descriptors[route.route.key];

    const isFocused = state.index === index;

    const onPress = () => {
      const event = navigation.emit({
        type: "tabPress",
        target: route.key,
      });

      if (!isFocused && !event.defaultPrevented) {
        navigation.navigate(route.name);
      }

      const onLongPress = () => {
        navigation.emit({
          type: "onLongPress",
          target: route.key,
        });
      };

      const inputRange = state.routes.map((_: any, i: number) => i);
      const opacity = Animated.interpolate(
        position,
        inputRange,
        inputRange.map((i: number) => (i === index ? 1 : 0))
      );

      return (
        <Pressable
          accessibilityRole="button"
          accessibilityState={isFocused ? { selected: true } : {}}
          accessibilityLabel={options.tabBarAccessibilityLabel}
          testID={options.tabBarTestID}
          onPress={onPress}
          onLongPress={onLongPress}
          key={route.key}>
          <Animated.Text style={{ opacity }}>hello</Animated.Text>
        </Pressable>
      );
    };
  };

  return (
    <PagerView initialPage={0} style={styles.container}>
      {state.route.map(renderScreens)}
    </PagerView>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 1,
    flexDirection: "row",
  },
});
