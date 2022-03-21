import * as React from "react";
import { StyleSheet, View, Pressable } from "react-native";

import { StackScreenProps } from "@react-navigation/stack";
import { LeftArrowIcon } from "assets/icons";
import { CustomPlainInput } from "components/forms/CustomPlainInput";
import { HeaderText } from "components/rnWrappers/headerText";
import { appContext, eventCreationContext } from "contexts/contextApi";
import { SafeAreaView } from "react-native-safe-area-context";
import { Colors, Sizing } from "styles/index";
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { EventCreationParamList } from "common/types/navigationTypes";

type Props = StackScreenProps<EventCreationParamList, "New Event Description">;

export const NewEventDescription = ({ navigation, route }: Props) => {
  const { colorScheme } = appContext();
  const { setTextContent } = eventCreationContext();
  const [eventTitle, setEventTitle] = React.useState<string>("");
  const [eventDescription, setEventsDescription] = React.useState<string>("");

  const isLightMode = colorScheme === "light";
  const isDisabledButton = !eventTitle || !eventDescription;

  // set the input values
  const onEventTitleChange = (val: string) => setEventTitle(val);
  const onDescriptionChange = (val: string) => setEventsDescription(val);

  // navigation handlers
  const onBackNavigationPress = () => navigation.goBack();
  const onNextPress = () => {
    setTextContent({ title: eventTitle, description: eventDescription });
    navigation.navigate("Available Days Selection");
  };

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
      <KeyboardAwareScrollView
        contentContainerStyle={{ height: "100%" }}
        style={{ width: "90%" }}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <HeaderText
          customStyles={{ marginBottom: Sizing.x10 }}
          colorScheme={colorScheme}>
          What would you like to name the event?
        </HeaderText>
        <CustomPlainInput
          label="Event Title"
          placeholder="How to scalpe a trade..."
          maxChar={40}
          onChangeCallback={onEventTitleChange}
        />
        <HeaderText
          customStyles={{ marginBottom: Sizing.x10 }}
          colorScheme={colorScheme}>
          Give a brief description of what services you will provide
        </HeaderText>
        <CustomPlainInput
          label="Description"
          maxChar={150}
          placeholder="I will be going over..."
          multiline
          numberOfLines={8}
          onChangeCallback={onDescriptionChange}
        />
        <FullWidthButton
          text="Next"
          colorScheme={colorScheme}
          disabled={isDisabledButton}
          onPressCallback={onNextPress}
          style={{ marginTop: "auto", marginBottom: Sizing.x15 }}
        />
      </KeyboardAwareScrollView>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    marginVertical: Sizing.x15,
    alignSelf: "flex-start",
  },
  eventInfoContainer: {
    marginBottom: Sizing.x25,
  },
});
