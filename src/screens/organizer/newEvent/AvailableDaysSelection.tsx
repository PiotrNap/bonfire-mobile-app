import * as React from "react";
import {
  View,
  StyleSheet,
  Pressable,
  ScrollView,
  Linking,
  Text,
} from "react-native";

import { CheckIcon, LeftArrowIcon } from "assets/icons";
import { appContext, eventCreationContext } from "contexts/contextApi";
import { SafeAreaView } from "react-native-safe-area-context";
import { Colors, Outlines, Sizing } from "styles/index";
import { HeaderText } from "components/rnWrappers/headerText";
import { StackScreenProps } from "@react-navigation/stack";
import { EventCreationParamList } from "common/types/navigationTypes";
import { MonthlyWrapper } from "components/calendar";
import { CalendarWrapperSimple } from "components/calendar/CalendarWrapperSimple";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { BodyText } from "components/rnWrappers/bodyText";
import { useGoogleAuth } from "lib/hooks/useGoogleAuth";
import { ErrorModal } from "components/modals/errorModal";
import * as qs from "qs";
import { fontWeight } from "../../../styles/typography";

type Props = StackScreenProps<
  EventCreationParamList,
  "Available Days Selection"
>;

export const AvailableDaysSelection = ({ navigation }: Props) => {
  const { colorScheme } = appContext();
  const {
    selectedDays,
    setDateFrame,
    removeSelectedDays,
    removeSelectedWeeks,
  } = eventCreationContext();
  const { isRequesting, isValidOauth, isLoading, requestAccess } =
    useGoogleAuth();
  const [acceptedCheckbox, setAcceptedChecbox] = React.useState<boolean>(false);
  const [error, setError] = React.useState<any>({ isVisible: false, type: "" });

  const listener = {
    start: function () {
      Linking.addEventListener("url", (event) => {
        const query = qs.parse(event.url.split("?")[1]);
        const { success } = query;

        if (success === "false") {
          setError({ isVisible: true, type: "GoogleOauth" });
        }

        if (success === "true") navigation.navigate("Available Time Selection");
      });
    },
    remove: function () {
      Linking.removeEventListener("url", this.start);
    },
  };

  React.useEffect(() => {
    listener.start();

    return () => listener.remove();
  }, []);

  const isLightMode = colorScheme === "light";
  const isDisabledBtn =
    selectedDays === null || !Object.entries(selectedDays).length;
  const onCheckBoxPress = () => {
    setError({ isVisible: false, type: "" });
    setAcceptedChecbox((prev) => !prev);
  };
  const onBackNavigationPress = React.useCallback(() => {
    removeSelectedDays();
    removeSelectedWeeks();
    setAcceptedChecbox(false);
    navigation.goBack();
  }, []);
  const onNextButtonPress = async () => {
    if (error.isVisible) setError({ isVisible: false, type: "" });

    if (acceptedCheckbox)
      try {
        await requestAccess();
      } catch (e) {}

    const selectedDaysKeys = Object.keys(selectedDays);
    setDateFrame(
      new Date(Number(selectedDaysKeys[0])),
      new Date(Number(selectedDaysKeys[selectedDaysKeys.length - 1]))
    );

    if (!acceptedCheckbox) navigation.navigate("Available Time Selection");
  };

  return (
    <>
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
                color={
                  isLightMode ? Colors.primary.s600 : Colors.primary.neutral
                }
              />
            </Pressable>
          </View>
          <View style={{ width: "90%" }}>
            <HeaderText
              customStyles={{ marginBottom: Sizing.x10 }}
              colorScheme={colorScheme}>
              Select dates you are available to host event
            </HeaderText>
          </View>
          <ScrollView showsVerticalScrollIndicator={false}>
            <CalendarWrapperSimple>
              <MonthlyWrapper isNewEventCalendar={true} />
            </CalendarWrapperSimple>
            <View style={styles.messageWrapper}>
              <BodyText
                customStyle={{
                  fontFamily: "Roboto-Regular",
                  fontSize: Sizing.x14,
                  width: "90%",
                }}
                colors={[Colors.primary.s800, Colors.primary.neutral]}>
                * <Text style={{ ...fontWeight.bold }}>Hint:</Text> Press on a
                day to select it or tap on a day name to select them all.
              </BodyText>
            </View>
            {!isValidOauth && !isLoading && (
              <View style={styles.messageWrapper}>
                <Pressable
                  onPress={onCheckBoxPress}
                  hitSlop={5}
                  style={[
                    styles.checkbox,
                    {
                      borderWidth: isLightMode ? Outlines.borderWidth.thin : 0,
                      backgroundColor:
                        isLightMode && acceptedCheckbox
                          ? Colors.primary.s600
                          : Colors.primary.neutral,
                      borderColor:
                        isLightMode && acceptedCheckbox
                          ? Colors.primary.s600
                          : "black",
                    },
                  ]}>
                  <CheckIcon
                    width="15"
                    height="15"
                    strokeWidth="3.5"
                    stroke={
                      isLightMode
                        ? Colors.primary.neutral
                        : !isLightMode && acceptedCheckbox
                        ? Colors.primary.s600
                        : Colors.primary.neutral
                    }
                  />
                </Pressable>
                <BodyText
                  customStyle={{
                    fontFamily: "Roboto-Regular",
                    fontSize: Sizing.x14,
                    width: "90%",
                  }}
                  colors={[Colors.primary.s800, Colors.primary.neutral]}>
                  Attendees schedule time on my Google calendar
                  {"\n"}
                  Next: grant access
                </BodyText>
              </View>
            )}
          </ScrollView>
          <FullWidthButton
            text="Next"
            colorScheme={colorScheme}
            disabled={isDisabledBtn}
            onPressCallback={onNextButtonPress}
            loadingIndicator={isRequesting}
            buttonType="filled"
            style={styles.button}
          />
        </View>
      </SafeAreaView>
      <ErrorModal isModalVisible={error.isVisible} errorType={error.type} />
    </>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    marginVertical: Sizing.x15,
    alignSelf: "center",
    width: "90%",
  },
  messageWrapper: {
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    marginTop: Sizing.x5,
  },
  checkbox: {
    alignItems: "center",
    justifyContent: "center",
    width: 17,
    height: 17,
    marginTop: Sizing.x5,
    marginRight: Sizing.x10,
    marginLeft: Sizing.x2,
    borderRadius: Sizing.x3,
  },
  button: { width: "90%", marginTop: "auto", marginBottom: Sizing.x15 },
});
