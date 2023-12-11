import * as React from "react"
import { StyleSheet, View, Pressable } from "react-native"

import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { StackScreenProps } from "@react-navigation/stack"
import Filter from "bad-words"

import { LeftArrowIcon } from "assets/icons"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { EventCreationParamList } from "common/types/navigationTypes"
import { Layout } from "components/layouts/basicLayout"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"
import { EventType, EventVisibility } from "common/interfaces/newEventInterface"
import { CustomInput } from "components/forms/CustomInput"
import { Field, Formik } from "formik"
import { newEventScheme } from "lib/validators"
import { formStyleDark, formStyleLight, inputStyles } from "../../../styles/forms"
import { PaymentTokensForm } from "components/forms/PaymentTokensForm"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { showErrorToast } from "lib/helpers"
import { AssetUnit } from "lib/wallet/types"
import { noop } from "lib/utils"
import { ToggleButton } from "./toggleButton"
import { utf8ToHex } from "lib/wallet/utils"

type Props = StackScreenProps<EventCreationParamList, "New Event Description">

export const NewEventDescription = ({ navigation }: Props) => {
  const { colorScheme } = appContext()
  const {
    setTextContent,
    setHourlyRate,
    setEventVisibility,
    setCancellation,
    visibility: eventVisibility,
    textContent,
    cancellation,
    hourlyRate,
  } = eventCreationContext()
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [title, setTitle] = React.useState<string>("")
  const [summary, setSummary] = React.useState<string>("")
  const [fee, setFee] = React.useState<number>(0)
  const [visibility, setVisibility] = React.useState<EventVisibility>("public")
  const [cancelWindow, setCancelWindow] = React.useState<number>(0)
  const [paymentTokensErrorStatus, setPaymentTokensErrorStatus] =
    React.useState<boolean>(false)
  const paymentTokensRef = React.useRef()
  const descriptionRef = React.useRef<any>()

  const isLightMode = colorScheme === "light"
  const isDisabledButton =
    paymentTokensErrorStatus ||
    descriptionRef?.current == null ||
    !descriptionRef.current.isValid ||
    !descriptionRef.current.dirty

  const formStyles = Object.assign(
    {},
    inputStyles,
    isLightMode ? formStyleLight : formStyleDark
  )

  const onBackNavigationPress = () => navigation.goBack()
  const checkIfPaymentTokensAreValid = async (paymentTokens: AssetUnit[]) => {
    // TODO after release check on our back-end if a given token exists on the blockchain
    return (
      paymentTokens &&
      paymentTokens.every((pt, idx) =>
        idx === 0
          ? !!pt.count
          : paymentTokens[idx].policyId &&
            Number(paymentTokens[idx].count) > 0 &&
            paymentTokens[idx].name
      )
    )
  }

  const ontitleChange = (val: string) => setTitle(val)
  const onSummaryChange = (summary) => setSummary(summary)
  const onCancelWindowChange = (hours) => setCancelWindow(hours)
  const onFeeChange = (fee) => setFee(fee)
  const onVisibilityTypeChange = (type: any) => setVisibility(type)
  const changePaymentTokensErrorStatus = (status) => setPaymentTokensErrorStatus(status)

  /** Check inputs and navigate to next screen **/
  const onNextPress = async () => {
    let paymentTokens: AssetUnit[] = paymentTokensRef.current?.values
    if (!paymentTokens) return showErrorToast("Missing payment tokens info")

    //update the unit 'name' with the user typed 'displayName'
    paymentTokens = paymentTokens.map((pt) => ({
      ...pt,
      name: utf8ToHex(pt.displayName || ""),
    }))

    const paymentTokensValid = await checkIfPaymentTokensAreValid(paymentTokens)
    if (!paymentTokensValid)
      return showErrorToast("Please double-check tokens input fields")

    const allTokensDivisibleByTwo = paymentTokens.every(
      (pt) => Number(pt.count) % 2 === 0
    )
    if (!allTokensDivisibleByTwo)
      return showErrorToast("Tokens quantity must be divisible by 2")

    // update event-creation-context
    setSubmitted(true)
    const bw = new Filter()
    if (bw.isProfane([title, summary].join(" "))) {
      showInappropriateContentModal()
      setSubmitted(false)
      return
    }

    setTextContent({ title, summary })
    setHourlyRate(paymentTokens)
    setCancellation({ fee, window: cancelWindow })
    setEventVisibility(visibility)
    navigation.navigate("Available Days Selection")
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
        <Formik
          innerRef={descriptionRef}
          validationSchema={newEventScheme()}
          initialValues={{
            title: textContent?.title || "",
            summary: textContent?.summary || "",
            fee: cancellation?.fee || 0,
          }}
          onSubmit={noop}>
          {({ handleSubmit, isValid, validateForm, values }) => (
            <>
              <HeaderText customStyles={styles.inputLabel} colorScheme={colorScheme}>
                What's the title?
              </HeaderText>
              <Field
                key="title"
                name="title"
                maxChar={40}
                customOnChange={ontitleChange}
                component={CustomInput}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
              <HeaderText customStyles={styles.inputLabel} colorScheme={colorScheme}>
                Short Summary
              </HeaderText>
              <Field
                key="summary"
                name="summary"
                component={CustomInput}
                maxChar={250}
                customOnChange={onSummaryChange}
                multiline
                numberOfLines={8}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
              <HeaderText customStyles={styles.inputLabel} colorScheme={colorScheme}>
                Event Visibility
              </HeaderText>
              <ToggleButton
                defaultValue={eventVisibility}
                options={["Public", "Private"]}
                values={["public", "private"]}
                animationDuration={30}
                onSelect={onVisibilityTypeChange}
              />
              <HeaderText customStyles={styles.inputLabel} colorScheme={colorScheme}>
                Cancellation
              </HeaderText>
              <SubHeaderText
                customStyle={styles.inputSubLabel}
                colors={[Colors.primary.s800, Colors.primary.neutral]}>
                Hours before start
              </SubHeaderText>
              <ToggleButton
                defaultValue={cancellation?.window}
                options={["0", "24", "48", "72"]}
                values={[0, 24, 48, 72]}
                animationDuration={30}
                onSelect={onCancelWindowChange}
              />
              <SubHeaderText
                customStyle={styles.inputSubLabel}
                colors={[Colors.primary.s800, Colors.primary.neutral]}>
                Fee (% of ADA)
              </SubHeaderText>
              <Field
                key="fee"
                name="fee"
                maxChar={40}
                defaultValue={cancellation?.fee}
                customOnChange={onFeeChange}
                component={CustomInput}
                submitted={submitted}
                validateForm={validateForm}
                styles={formStyles}
              />
            </>
          )}
        </Formik>
        <HeaderText customStyles={styles.inputLabel} colorScheme={colorScheme}>
          Your Rate/hr
        </HeaderText>
        <PaymentTokensForm
          ref={paymentTokensRef}
          errorStatus={paymentTokensErrorStatus}
          changeErrorStatus={changePaymentTokensErrorStatus}
          defaultValues={hourlyRate}
        />
        <FullWidthButton
          text="Next"
          colorScheme={colorScheme}
          disabled={isDisabledButton}
          onPressCallback={onNextPress}
          style={{ marginVertical: Sizing.x15 }}
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
  inputLabel: {
    marginBottom: Sizing.x5,
  },
  inputSubLabel: {
    flex: 1,
    ...Typography.subHeader.x10,
    paddingLeft: Sizing.x5,
    paddingBottom: Sizing.x5,
  },
})
