import * as React from "react";
import { View, Text, StyleSheet, Pressable } from "react-native";

import { SafeAreaView } from "react-native-safe-area-context";
import { StackScreenProps } from "@react-navigation/stack";
import { Colors, Sizing, Typography } from "styles/index";
import { appContext } from "contexts/contextApi";

import { FullWidthButton } from "components/buttons/fullWidthButton";
import { BodyText } from "components/rnWrappers/bodyText";
import { LeftArrowIcon, PaymentSuccessfulIcon } from "assets/icons";
import { AppStackParamList } from "common/types/navigationTypes";

export interface ConfirmationProps {}

type Props = StackScreenProps<AppStackParamList, "Confirmation">;

export const Confirmation = ({ navigation, route }: Props) => {
  const { colorScheme } = appContext();

  const isLightMode = colorScheme === "light";

  const navigateBack = () => {
    if (route.params?.isBookingWalletTopUp != null) {
      navigation.navigate("Duration Choice");
    }
    if (route.params?.isBookingConfirmation != null) {
      navigation.reset({
        index: 0,
        routes: [{ name: "Navigation Screens" }],
      });
    }
  };

  const onButtonPress = () => navigateBack();
  const onBackNavigationPress = () => navigateBack();

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode
            ? Colors.primary.neutral
            : Colors.primary.s600,
        },
      ]}>
      <View style={styles.navigation}>
        <Pressable onPress={onBackNavigationPress} hitSlop={10}>
          <LeftArrowIcon
            width={24}
            height={24}
            color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
          />
        </Pressable>
      </View>
      <View style={styles.container}>
        <PaymentSuccessfulIcon width={200} height={200} style={styles.icon} />
        <View style={styles.textContainer}>
          <Text
            style={
              isLightMode ? styles.headerText_light : styles.headerText_dark
            }>
            You're all set!
          </Text>
          <BodyText colors={[Colors.primary.s600, Colors.primary.neutral]}>
            Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum
            venenatis quam sem, eget bibendum lorem convallis et. Donec velit
            ante.
          </BodyText>
        </View>
        <View style={styles.buttonContainer}>
          <FullWidthButton
            onPressCallback={onButtonPress}
            text={"Go to dashboard"}
            colorScheme={colorScheme}
          />
        </View>
      </View>
    </SafeAreaView>
  );
};

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
});
