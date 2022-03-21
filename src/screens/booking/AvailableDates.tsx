import * as React from "react";
import { View, StyleSheet, Pressable, Text } from "react-native";

import { Colors, Sizing, Typography } from "styles/index";
import { OrganizerProfile } from "components/booking/index";
import { LeftArrowIcon } from "icons/index";
import {
  appContext,
  bookingContext,
  myCalendarContext,
} from "contexts/contextApi";

import { SafeAreaView } from "react-native-safe-area-context";
import { customAvailabilities } from "../../api_data/customAvailabilities";
import { featuredOrganizers } from "../../api_data/featuredOrganizers";
import { EventsList } from "components/booking/EventsList";
import { BookingStackParamList } from "common/types/navigationTypes";
import { StackScreenProps } from "@react-navigation/stack";

export interface AvailableDatesProps {}

type Props = StackScreenProps<BookingStackParamList, "Available Dates">;

export const AvailableDates = ({ navigation, route }: Props) => {
  const { colorScheme } = appContext();
  const { setAvailCalendar } = myCalendarContext();
  const { setPreviewingOrganizer, previewingOrganizer } = bookingContext();
  const alias = route.params?.alias;
  const isLightMode = colorScheme === "light";

  React.useEffect(() => {
    let profile = featuredOrganizers.items.find((org) => org.alias === alias);

    setAvailCalendar(customAvailabilities);
    setPreviewingOrganizer(profile);
  }, [navigation, route.params]);

  const onBackNavigationPress = () => navigation.goBack();

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
      <View style={{ flex: 1, width: "100%", alignItems: "center" }}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <OrganizerProfile profile={previewingOrganizer} />
        <View style={styles.timesHeader}>
          <Text
            style={
              isLightMode
                ? styles.timesHeaderText_light
                : styles.timesHeaderText_dark
            }>
            Currently available
          </Text>
        </View>
        <View style={{ flex: 1, width: "90%" }}>
          <EventsList />
        </View>
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  calendarHeader: {
    width: "80%",
    marginVertical: Sizing.x5,
    flexDirection: "row",
    justifyContent: "space-between",
  },
  calendarHeaderText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  calendarHeaderText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
  calendarWrapper: {
    flex: 1,
    width: "100%",
    alignItems: "center",
  },
  buttonContainer: {
    alignItems: "center",
    justifyContent: "center",
    width: "90%",
    marginVertical: Sizing.x10,
  },
  timesHeader: {
    marginVertical: Sizing.x5,
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
});
