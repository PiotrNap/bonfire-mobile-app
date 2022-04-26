import * as React from "react"
import { StyleProp, StyleSheet, View } from "react-native"

import { Formik, Field } from "formik"
import Filter from "bad-words"

import { accountValidationScheme } from "lib/utils"
import { Colors, Forms, Outlines, Sizing, Typography } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useUpdateAccountInfo } from "lib/hooks/useUpdateAccountInfo"
import { appContext } from "contexts/contextApi"
import { CustomInput } from "./CustomInput"
import {
  showFailedModal,
  showInappropriateContentModal,
} from "lib/modalAlertsHelpers"

interface Props {
  accountType: "organizer" | "attendee"
  onInfoHasChanged: (val: boolean) => void
  onUpdateResponse: (val: { msg: string; status: number }) => void
  userInfo: any
}

export const UpdateAccountForm = ({
  accountType,
  onUpdateResponse,
  userInfo,
}: Props) => {
  const { isLoading, setIsLoading, msg, updateAccountInfo } =
    useUpdateAccountInfo()
  const { colorScheme } = appContext()
  const [submitted, setSubmitted] = React.useState<boolean>(false)

  const isLightMode = colorScheme === "light"
  const isAttendee = accountType === "attendee"
  const handleSubmit = async (newValues: any) => {
    const bw = new Filter()
    const words = Object.values(newValues).join(" ")
    if (bw.isProfane(words)) return showInappropriateContentModal()

    var hasChanged: boolean = false

    if (accountType === "organizer") {
      newValues.hourlyRate = Number(newValues.hourlyRate)
      for (let k of Object.keys(newValues)) {
        if (newValues[k] !== userInfo[k]) hasChanged = true
      }
    } else {
      if (
        newValues.name !== userInfo.name ||
        newValues.username !== userInfo.username
      )
        hasChanged = true
    }
    if (!hasChanged) return

    setIsLoading(true)
    const res = await updateAccountInfo(newValues, userInfo.id)
    res && onUpdateResponse(res)
  }
  const onChangeCallback = (val: any) => {}
  let formStyles: StyleProp<any>

  if (isLightMode) {
    formStyles = Object.assign({}, inputStyles, styles, formStyleLight)
  } else {
    formStyles = Object.assign({}, inputStyles, styles, formStyleDark)
  }

  return (
    <Formik
      validationSchema={accountValidationScheme()}
      validateOnChange={submitted}
      validateOnBlur={submitted}
      initialValues={userInfo}
      onSubmit={handleSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="name"
            name="name"
            label="Name"
            component={CustomInput}
            onChange={onChangeCallback}
            defaultValue={userInfo.name}
            keyboardType="default"
            textContentType="name"
            autoCompleteType="name"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
          />
          <Field
            key="username"
            name="username"
            label="Username"
            defaultValue={userInfo.username}
            component={CustomInput}
            onChange={onChangeCallback}
            keyboardType="default"
            textContentType="username"
            autoCompleteType="username"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
          />
          {!isAttendee && (
            <>
              <Field
                key="profession"
                name="profession"
                label="Profession"
                spellCheck={true}
                defaultValue={userInfo.profession}
                component={CustomInput}
                onChange={onChangeCallback}
                keyboardType="default"
                validateForm={validateForm}
                submitted={submitted}
                styles={formStyles}
              />
              <Field
                key="jobTitle"
                name="jobTitle"
                label="Job Title"
                defaultValue={userInfo.jobTitle}
                component={CustomInput}
                onChange={onChangeCallback}
                keyboardType="default"
                textContentType="jobTitle"
                validateForm={validateForm}
                submitted={submitted}
                styles={formStyles}
              />
              <Field
                key="bio"
                name="bio"
                label="About Yourself"
                defaultValue={userInfo.bio}
                spellCheck={true}
                multiline={true}
                numberOfLines={3}
                maxChar={250}
                component={CustomInput}
                onChange={onChangeCallback}
                keyboardType="default"
                validateForm={validateForm}
                submitted={submitted}
                styles={formStyles}
              />
              <Field
                key="skills"
                name="skills"
                label="Skills"
                defaultValue={userInfo.skills}
                spellCheck={true}
                component={CustomInput}
                onChange={onChangeCallback}
                keyboardType="default"
                validateForm={validateForm}
                submitted={submitted}
                styles={formStyles}
              />
              <Field
                id="hourlyRate"
                key="hourlyRate"
                name="hourlyRate"
                label="Hourly Rate (ADA)"
                defaultValue={String(userInfo.hourlyRate ?? 0)}
                component={CustomInput}
                onChange={onChangeCallback}
                keyboardType="numeric"
                validateForm={validateForm}
                submitted={submitted}
                styles={formStyles}
              />
            </>
          )}
          <View style={styles.buttonWrapper}>
            <FullWidthButton
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
              onPressCallback={handleSubmit}
              text={"Save Changes"}
              disabled={!isValid}
            />
          </View>
        </>
      )}
    </Formik>
  )
}

const styles = StyleSheet.create({
  buttonWrapper: {
    marginBottom: Sizing.x10,
  },
})

/**
 * Styles passed as props to CustomInput
 */
const inputStyles = StyleSheet.create({
  inputContainer: {
    width: "100%",
    alignItems: "center",
  },
  labelContainer: {
    width: "100%",
  },
  textInputWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
  },
  input: {
    width: "100%",
    ...Forms.input.primary,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
  errorInput: {
    borderColor: Colors.danger.s300,
  },
  errorWrapper: {
    justifyContent: "center",
    alignItems: "center",
    alignSelf: "center",
    height: Sizing.x20,
  },
  error: {
    color: Colors.danger.s400,
    ...Typography.header.x20,
  },
})

const formStyleLight = StyleSheet.create({
  label: {
    ...Forms.inputLabel.primary_light,
  },
  input: {
    width: "100%",
    ...Forms.input.primary_light,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
})

const formStyleDark = StyleSheet.create({
  label: {
    ...Forms.inputLabel.primary_dark,
  },
  input: {
    width: "100%",
    ...Forms.input.primary_dark,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
})
