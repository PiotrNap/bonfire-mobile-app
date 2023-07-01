import * as React from "react"
import { StyleSheet, View, Pressable } from "react-native"

import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { StackScreenProps } from "@react-navigation/stack"
import Filter from "bad-words"

import { LeftArrowIcon } from "assets/icons"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { EventCreationParamList } from "common/types/navigationTypes"
import { Layout } from "components/layouts/basicLayout"
import { BodyText } from "components/rnWrappers/bodyText"
import { ProfileContext } from "contexts/profileContext"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"
import { EventType, HourlyRate } from "common/interfaces/newEventInterface"
import { Checkbox } from "components/forms/Checkbox"
import ToggleButton from "./toggleButton"
import { CustomInput } from "components/forms/CustomInput"
import { Field, Formik } from "formik"
import { newEventScheme } from "lib/validators"
import {
  formStyleDark,
  formStyleLight,
  inputStyles,
} from "../../../styles/forms"

type Props = StackScreenProps<EventCreationParamList, "New Event Description">

export const NewEventDescription = ({ navigation }: Props) => {
  const { colorScheme } = appContext()
  const { hourlyRate } = React.useContext(ProfileContext)
  const { setTextContent, setHourlyRate, setEventType, setPrivateEvent } =
    eventCreationContext()
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [eventTitle, setEventTitle] = React.useState<string>("")
  const [_eventType, _setEventType] = React.useState<EventType>("One-Time")
  const [_privateEvent, _setPrivateEvent] = React.useState<boolean>(false)
  const [_hourlyRate, _setHourlyRate] = React.useState<HourlyRate>({
    ada: 0,
    gimbals: 0,
  })
  const [markedCheckbox, setMarkedCheckbox] = React.useState<boolean>(false)

  const isLightMode = colorScheme === "light"
  const isDisabledButton = !eventTitle

  // set the input values
  const onEventTitleChange = (val: string) => setEventTitle(val)
  const onAdaHourlyRateChange = (val: string) => onHourlyRateChange(val, "ada")
  const onGimbalsHourlyRateChange = (val: string) =>
    onHourlyRateChange(val, "gimbals")
  const onHourlyRateChange = (val: any, type: "ada" | "gimbals") => {
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
  const onSubmit = (formValues: any) => {
    const { title, description, ada, gimbals } = formValues
    setSubmitted(true)
    const bw = new Filter()
    if (bw.isProfane([title, description].join(" "))) {
      showInappropriateContentModal()
      setSubmitted(false)
      return
    }

    setTextContent({ title, description })
    if (!markedCheckbox)
      setHourlyRate({
        ada: Number(ada),
        gimbals: Number(gimbals),
      })
    setEventType(_eventType)
    setPrivateEvent(_privateEvent)
    navigation.navigate("Available Days Selection")
  }
  const onCheckBoxPress = () => {
    setMarkedCheckbox((prev) => !prev)
    setHourlyRate(hourlyRate)
  }
  const onEventTypeChange = (type: any) => _setEventType(type)
  const onEventVisibilityChange = (type: any) =>
    _setPrivateEvent((prev) => !prev)
  const formStyles = Object.assign(
    {},
    inputStyles,
    isLightMode ? formStyleLight : formStyleDark
  )

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
        <Formik
          validationSchema={newEventScheme()}
          initialValues={{
            name: "",
            username: "",
          }}
          onSubmit={onSubmit}>
          {({ handleSubmit, isValid, validateForm }) => (
            <>
              <HeaderText
                customStyles={{ marginBottom: Sizing.x10 }}
                colorScheme={colorScheme}>
                What's the Event?
              </HeaderText>
              <Field
                key="title"
                name="title"
                label="Title"
                maxChar={40}
                customOnChange={onEventTitleChange}
                component={CustomInput}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
              <HeaderText
                customStyles={{ marginBottom: Sizing.x10 }}
                colorScheme={colorScheme}>
                Short Summary? (optional)
              </HeaderText>
              <Field
                key="description"
                name="description"
                label="Description"
                component={CustomInput}
                maxChar={250}
                multiline
                numberOfLines={8}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
              <HeaderText
                customStyles={{ marginBottom: Sizing.x10 }}
                colorScheme={colorScheme}>
                Select Type
              </HeaderText>
              <ToggleButton
                options={["One-Time", "Recurring"]}
                animationDuration={250}
                onSelect={onEventTypeChange}
              />
              <ToggleButton
                options={["Public", "Private"]}
                animationDuration={250}
                onSelect={onEventVisibilityChange}
              />
              <HeaderText
                customStyles={{ marginBottom: Sizing.x10 }}
                colorScheme={colorScheme}>
                Your Rate/hr
              </HeaderText>
              <Field
                component={CustomInput}
                key="ada"
                name="ada"
                label="Ada"
                keyboardType="numeric"
                defaultValue={String(_hourlyRate.ada)}
                customOnChange={onAdaHourlyRateChange}
                isDisabled={markedCheckbox}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
              <Field
                component={CustomInput}
                key="gimbals"
                name="gimbals"
                label="Gimbals"
                keyboardType="numeric"
                customOnChange={onGimbalsHourlyRateChange}
                defaultValue={String(_hourlyRate.gimbals)}
                onEndEditingCallback={onGimbalsHourlyRateChange}
                isDisabled={markedCheckbox}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
              {hourlyRate?.ada ? (
                <View style={styles.checkboxWrapper}>
                  <Checkbox
                    onCheckBoxPress={onCheckBoxPress}
                    acceptedCheckbox={markedCheckbox}>
                    <BodyText
                      customStyle={{
                        fontFamily: "Roboto-Medium",
                        fontSize: Sizing.x15,
                        width: "90%",
                      }}
                      changingColorScheme
                      colors={[Colors.primary.s800, Colors.primary.neutral]}>
                      Use my profile's default rate
                    </BodyText>
                  </Checkbox>
                </View>
              ) : (
                <></>
              )}

              <FullWidthButton
                text="Next"
                colorScheme={colorScheme}
                disabled={isDisabledButton}
                onPressCallback={handleSubmit}
                style={{ marginVertical: Sizing.x15 }}
              />
            </>
          )}
        </Formik>
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
  checkboxWrapper: {
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    alignItems: "center",
    marginBottom: Sizing.x5,
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
