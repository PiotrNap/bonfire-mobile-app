import * as React from "react";
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  ImageBackground,
  ScrollView,
} from "react-native";

import { StackScreenProps } from "@react-navigation/stack";
import { BookingStackParamList } from "common/types/navigationTypes";
import { Colors, Outlines, Sizing, Typography } from "styles/index";
import {
  SafeAreaView,
  useSafeAreaInsets,
} from "react-native-safe-area-context";
import { appContext, bookingContext } from "contexts/contextApi";
import { LeftArrowIcon } from "assets/icons";
import { MonthlyWrapper } from "components/calendar";
import { FullWidthButton } from "components/buttons/fullWidthButton";

type Props = StackScreenProps<
  BookingStackParamList,
  "Available Event Days Selection"
>;

export const AvailableDaysSelection = ({ navigation, route }: Props) => {
  const { title, image, color, titleColor } = route.params;
  const { colorScheme } = appContext();
  const { pickedDate, resetState } = bookingContext();

  const isLightMode = colorScheme !== "dark";
  const isDisabled = pickedDate === null;
  const insets = useSafeAreaInsets();

  //@TODO add the organizer info to route params
  const onBackNavigationPress = () => {
    resetState();
    navigation.navigate("Browse");
  };
  const onNextPress = () =>
    navigation.navigate("Available Times", route.params);

  return (
    <SafeAreaView style={{ flex: 1, paddingBottom: insets.bottom }}>
      <View style={styles.topContainer}>
        <ImageBackground
          resizeMode="cover"
          source={{ uri: image }}
          style={styles.backgroundImage}>
          <View style={[styles.topInnerContainer, { backgroundColor: color }]}>
            <View style={[styles.topInnerWrapper, { paddingTop: insets.top }]}>
              <View style={styles.navigation}>
                <Pressable onPress={onBackNavigationPress} hitSlop={10}>
                  <LeftArrowIcon
                    width={24}
                    height={24}
                    color={Colors.primary.neutral}
                  />
                </Pressable>
              </View>
            </View>
            <View
              style={[
                styles.eventTitleWrapper,
                { paddingBottom: insets.bottom + Sizing.x15 },
              ]}>
              <Text
                ellipsizeMode="tail"
                numberOfLines={2}
                style={[styles.eventTitle, { color: titleColor }]}>
                {title}
              </Text>
            </View>
          </View>
        </ImageBackground>
      </View>
      <ScrollView
        contentContainerStyle={[
          styles.bottomContainer,
          {
            backgroundColor: isLightMode
              ? Colors.primary.neutral
              : Colors.primary.s800,
          },
        ]}>
        <View style={styles.timesHeader}>
          <Text
            style={
              isLightMode
                ? styles.timesHeaderText_light
                : styles.timesHeaderText_dark
            }>
            Select an available day
          </Text>
        </View>
        <View style={styles.calendarWrapper}>
          <MonthlyWrapper isBookingCalendar={true} />
        </View>
        <View style={styles.buttonContainer}>
          <FullWidthButton
            onPressCallback={onNextPress}
            text={"Next Step"}
            colorScheme={colorScheme}
            disabled={isDisabled}
          />
        </View>
      </ScrollView>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  topContainer: {
    height: Sizing.x100,
  },
  bottomContainer: {
    flexGrow: 1,
    alignItems: "center",
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  bottomWrapper: {
    flex: 1,
    width: "90%",
    paddingVertical: Sizing.x20,
    justifyContent: "space-between",
  },
  calendarWrapper: {
    flex: 1,
  },
  buttonContainer: {
    alignItems: "center",
    justifyContent: "center",
    width: "90%",
    marginVertical: Sizing.x10,
  },
  timesHeader: {
    marginTop: Sizing.x25,
    marginBottom: Sizing.x10,
    marginRight: "auto",
    marginLeft: Sizing.x25,
  },
  timesHeaderText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  timesHeaderText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
  backgroundImage: {
    width: "100%",
    height: Sizing.x120,
    position: "absolute",
    top: 0,
  },
  topInnerContainer: {
    height: "100%",
    alignItems: "center",
    justifyContent: "flex-start",
    paddingBottom: Sizing.x15,
  },
  topInnerWrapper: {
    width: "90%",
    flexDirection: "row",
    justifyContent: "space-between",
  },
  eventTitleWrapper: {
    width: "90%",
    marginTop: "auto",
    marginBottom: Sizing.x20,
  },
  eventTitle: {
    ...Typography.header.x55,
    color: Colors.primary.neutral,
  },
});
