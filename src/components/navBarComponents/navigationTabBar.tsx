import * as React from "react";
import { View, Text, Pressable, StyleSheet } from "react-native";

import {
  CalendarIcon,
  SearchIcon,
  HomeIcon,
  UserIcon,
  WalletIcon,
} from "icons/index";
import { Colors, Typography, Outlines } from "styles/index";
import { OrganizerTabParamList } from "common/types/navigationTypes";
import { BottomTabBarProps } from "@react-navigation/bottom-tabs";
import { appContext } from "contexts/contextApi";
import { useSafeAreaInsets } from "react-native-safe-area-context";

type NavigationTabBarProps = BottomTabBarProps<OrganizerTabParamList>;

export const NavigationTabBar = ({
  state,
  descriptors,
  navigation,
}: NavigationTabBarProps) => {
  const { colorScheme } = appContext();
  const { bottom: bottomInset } = useSafeAreaInsets();

  const getNavBarIcon = (routeName: string) => {
    switch (routeName) {
      case "Home":
        return HomeIcon;
      case "Browse":
        return SearchIcon;
      case "Wallet":
        return WalletIcon;
      case "My Events":
        return CalendarIcon;
      case "Profile":
        return UserIcon;
      default:
        throw Error("Route name not supported");
    }
  };

  const label = (options: any, route: any) => {
    return options.tabBarLabel != null
      ? options.tabBarLable
      : options.title != null
      ? options.title
      : route.name;
  };

  const renderTabItem = (route: any, index: any) => {
    const { options } = descriptors[route.key];

    const isFocused = state.index === index;
    const Icon = getNavBarIcon(route.name);

    const onPress = () => {
      if (route.key != null) {
        //@ts-ignore
        const event = navigation.emit({
          type: "tabPress",
          target: route.key,
        });

        if (!isFocused && !event.defaultPrevented) {
          navigation.navigate(route.name);
        }
      }
    };

    const onLongPress = () => {
      navigation.emit({
        type: "tabLongPress",
        target: route.key,
      });
    };

    const navBarButtonStyle = () => {
      if (colorScheme == "light" && isFocused) {
        return [
          styles.navBarButton_light,
          { backgroundColor: Colors.primary.s800 },
        ];
      }
      if (colorScheme == "light" && !isFocused) {
        return [
          styles.navBarButton_light,
          { backgroundColor: Colors.primary.s200 },
        ];
      }
      if (colorScheme == "dark" && isFocused) {
        return [
          styles.navBarButton_dark,
          { backgroundColor: Colors.primary.neutral },
        ];
      }
      if (colorScheme == "dark" && !isFocused) {
        return [
          styles.navBarButton_dark,
          { backgroundColor: Colors.primary.brand },
        ];
      }
    };

    const iconStyle = () => {
      if (colorScheme == "light" && isFocused) {
        return Colors.primary.s200;
      }
      if (colorScheme == "light" && !isFocused) {
        return Colors.primary.s600;
      }
      if (colorScheme == "dark" && isFocused) {
        return Colors.primary.s600;
      }
      if (colorScheme == "dark" && !isFocused) {
        return Colors.primary.s200;
      }
    };

    return (
      <View style={styles.navBarButtonWrapper} key={index}>
        <Pressable
          accessibilityRole="button"
          accessibilityState={isFocused ? { selected: true } : {}}
          accessibilityLabel={options.tabBarAccessibilityLabel}
          testID={options.tabBarTestID}
          onPress={onPress}
          onLongPress={onLongPress}
          key={route.key}
          style={[
            navBarButtonStyle(),
            isFocused ? { ...Outlines.shadow.lifted } : {},
          ]}>
          <Icon width={24} height={24} stroke={iconStyle()} strokeWidth={2} />
        </Pressable>
        <Text
          style={
            colorScheme === "light"
              ? styles.navBarButtonLabel_light
              : styles.navBarButtonLabel_dark
          }>
          {label(options, route)}
        </Text>
      </View>
    );
  };
  return (
    <View
      style={[
        { marginBottom: bottomInset },
        colorScheme == "light" ? styles.container_light : styles.container_dark,
      ]}>
      {state.routes.map(renderTabItem)}
    </View>
  );
};

const styles = StyleSheet.create({
  container_light: {
    flexDirection: "row",
    backgroundColor: Colors.primary.neutral,
  },
  container_dark: {
    flexDirection: "row",
    borderTopWidth: Outlines.borderWidth.hairline,
    borderTopColor: Colors.primary.brand,
    backgroundColor: Colors.primary.s600,
  },
  navBarButtonWrapper: {
    flex: 1,
    alignItems: "center",
  },
  navBarButton_light: {
    alignItems: "center",
    justifyContent: "center",
    borderWidth: 0.8,
    borderColor: Colors.primary.neutral,
    borderRadius: 10,
    width: 44,
    height: 44,
    margin: 5,
  },
  navBarButton_dark: {
    alignItems: "center",
    justifyContent: "center",
    borderWidth: 0.8,
    borderColor: Colors.primary.brand,
    borderRadius: 10,
    width: 44,
    height: 44,
    margin: 5,
  },
  navBarButtonLabel_light: {
    color: Colors.primary.s600,
    ...Typography.header.x5,
  },
  navBarButtonLabel_dark: {
    color: Colors.primary.s400,
    ...Typography.subHeader.x5,
  },
});
