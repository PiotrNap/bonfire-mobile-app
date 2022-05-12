import * as React from "react"
import { StyleSheet, View, Pressable } from "react-native"

import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { StackScreenProps } from "@react-navigation/stack"
import Filter from "bad-words"
import ButtonToggle from "react-native-button-toggle-group"

import { CheckIcon, LeftArrowIcon } from "assets/icons"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { EventCreationParamList } from "common/types/navigationTypes"
import { Layout } from "components/layouts/basicLayout"
import { BodyText } from "components/rnWrappers/bodyText"
import { ProfileContext } from "contexts/profileContext"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"
import { applyOpacity } from "../../../styles/colors"
import { EventType, HourlyRate } from "common/interfaces/newEventInterface"

type Props = StackScreenProps<EventCreationParamList, "New Event Description">

export const NewEventDescription = ({ navigation }: Props) => {
  const { colorScheme } = appContext()
  const { hourlyRate } = React.useContext(ProfileContext)
  console.log(hourlyRate)
  const { setTextContent, setHourlyRate, setEventType } = eventCreationContext()
  const [eventTitle, setEventTitle] = React.useState<string>("")
  const [eventType, _setEventType] = React.useState<EventType>("One-Time")
  const [_hourlyRate, _setHourlyRate] = React.useState<HourlyRate>({
    ada: 0,
    gimbals: 0,
  })
  const [eventDescription, setEventsDescription] = React.useState<string>("")
  const [markedCheckbox, setMarkedCheckbox] = React.useState<boolean>(false)

  const isLightMode = colorScheme === "light"
  const isDisabledButton =
    !eventTitle || !eventDescription || (!+_hourlyRate && !markedCheckbox)

  // set the input values
  const onEventTitleChange = (val: string) => setEventTitle(val)
  const onDescriptionChange = (val: string) => setEventsDescription(val)
  const onAdaHourlyRateChange = (val: string) => onHourlyRateChange(val, "ada")
  const onGimbalsHourlyRateChange = (val: string) =>
    onHourlyRateChange(val, "gimbals")
  const onHourlyRateChange = (val: string, type: "ada" | "gimbals") => {
    if (typeof +val === "number" && +val > 0) {
      if (type === "ada") {
        _setHourlyRate({ ..._hourlyRate, ada: Number(val) })
      } else _setHourlyRate({ ..._hourlyRate, gimbals: Number(val) })
    } else {
      if (type === "ada") {
        _setHourlyRate({ ..._hourlyRate, ada: 0 })
      } else _setHourlyRate({ ..._hourlyRate, gimbals: 0 })
    }
  }
  // navigation handlers
  const onBackNavigationPress = () => navigation.goBack()
  const onNextPress = () => {
    const bw = new Filter()
    if (bw.isProfane([eventTitle, eventDescription].join(" ")))
      return showInappropriateContentModal()

    setTextContent({ title: eventTitle, description: eventDescription })
    if (!markedCheckbox)
      setHourlyRate({
        ada: Number(_hourlyRate.ada),
        gimbals: Number(_hourlyRate.gimbals),
      })
    setEventType(eventType)

    navigation.navigate("Available Days Selection")
  }
  const onCheckBoxPress = () => {
    setMarkedCheckbox((prev) => !prev)
    setHourlyRate(hourlyRate)
  }

  return (
    <Layout scrollable>
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
          maxChar={40}
          onEndEditingCallback={onEventTitleChange}
        />
        <HeaderText
          customStyles={{ marginBottom: Sizing.x10 }}
          colorScheme={colorScheme}>
          Give a brief description of what services you will provide
        </HeaderText>
        <CustomPlainInput
          label="Description"
          maxChar={250}
          multiline
          numberOfLines={8}
          onEndEditingCallback={onDescriptionChange}
        />
        <HeaderText
          customStyles={{ marginBottom: Sizing.x10 }}
          colorScheme={colorScheme}>
          Specify your hourly rate
        </HeaderText>
        <CustomPlainInput
          label="Ada/hour"
          keyboardType="numeric"
          defaultValue={String(_hourlyRate.ada)}
          onEndEditingCallback={onAdaHourlyRateChange}
          isDisabled={markedCheckbox}
        />
        <CustomPlainInput
          label="Gimbals/hour"
          keyboardType="numeric"
          defaultValue={String(_hourlyRate.gimbals)}
          onEndEditingCallback={onGimbalsHourlyRateChange}
          isDisabled={markedCheckbox}
        />
        {(!hourlyRate?.ada || !hourlyRate.gimbals) && (
          <View style={styles.messageWrapper}>
            <Pressable
              onPress={onCheckBoxPress}
              hitSlop={5}
              style={[
                styles.checkbox,
                {
                  borderWidth: isLightMode ? Outlines.borderWidth.thin : 0,
                  backgroundColor:
                    isLightMode && markedCheckbox
                      ? Colors.primary.s600
                      : Colors.primary.neutral,
                  borderColor:
                    isLightMode && markedCheckbox
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
                    : !isLightMode && markedCheckbox
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
              changingColorScheme
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              Use my profile default rate
            </BodyText>
          </View>
        )}
        <FullWidthButton
          text="Next"
          colorScheme={colorScheme}
          disabled={isDisabledButton}
          onPressCallback={onNextPress}
          style={{ marginTop: Sizing.x15, marginBottom: Sizing.x15 }}
        />
      </KeyboardAwareScrollView>
    </Layout>
  )
}

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
})
