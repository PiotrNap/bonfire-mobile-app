import * as React from "react"
import { StyleSheet } from "react-native"

import { Formik, Field } from "formik"

import { accountValidationScheme } from "lib/utils"
import { Colors, Forms, Outlines, Sizing, Buttons } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useUpdateAccountInfo } from "lib/hooks/useUpdateAccountInfo"
import { ProfileContext } from "contexts/profileContext"
import { appContext } from "contexts/contextApi"
import { CustomPlainInput } from "./CustomPlainInput"

interface Props {
  accountType: "organizer" | "attendee"
  onInfoHasChanged: (val: boolean) => void
}

export const UpdateAccountForm = ({ accountType, onInfoHasChanged }: Props) => {
  const { isLoading, isUpdated, error, updateAccountInfo } =
    useUpdateAccountInfo()
  const { username, name, profession, bio, hourlyRate, skills, jobTitle } =
    React.useContext(ProfileContext)
  const { colorScheme } = appContext()
  const [submitted, setSubmitted] = React.useState<boolean>(false)

  const isLightMode = colorScheme === "light"
  const isAttendee = accountType === "attendee"

  const attInitialValues = {
    name,
    username,
  }

  const orgInitialValues = {
    ...attInitialValues,
    profession,
    bio,
    hourlyRate,
    skills,
    jobTitle,
  }
  console.log(name, username)

  const handleSubmit = (val) => {
    console.log("submitted ", val)
  }

  const fieldStyles = {
    ...inputStyles,
    ...(isLightMode ? inputStyles_light : inputStyles_dark),
  }

  const onChangeCallback = (val: any) => console.log(val.memoizedProps)
  return (
    <Formik
      validationSchema={accountValidationScheme()}
      validateOnChange={submitted}
      validateOnBlur={submitted}
      initialValues={isAttendee ? attInitialValues : orgInitialValues}
      onSubmit={handleSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="name"
            name="name"
            label="Name"
            defaultValue={name}
            component={CustomPlainInput}
            onChange={onChangeCallback}
            keyboardType="default"
            textContentType="name"
            autoCompleteType="name"
            validateForm={validateForm}
            submitted={submitted}
            styles={inputStyles}
          />
          <Field
            key="username"
            name="username"
            label="Username"
            defaultValue={username}
            component={CustomPlainInput}
            onChange={onChangeCallback}
            keyboardType="default"
            textContentType="username"
            autoCompleteType="username"
            validateForm={validateForm}
            submitted={submitted}
            styles={inputStyles}
          />
          {!isAttendee && (
            <>
              <Field
                key="profession"
                name="profession"
                label="Profession"
                spellCheck={true}
                defaultValue={profession}
                component={CustomPlainInput}
                onChange={onChangeCallback}
                keyboardType="default"
                validateForm={validateForm}
                submitted={submitted}
                styles={inputStyles}
              />
              <Field
                key="jobTitle"
                name="jobTitle"
                label="Job Title"
                defaultValue={jobTitle}
                component={CustomPlainInput}
                onChange={onChangeCallback}
                keyboardType="default"
                textContentType="jobTitle"
                validateForm={validateForm}
                submitted={submitted}
                styles={inputStyles}
              />
              <Field
                key="bio"
                name="bio"
                label="About yourself"
                defaultValue={bio}
                spellCheck={true}
                multiline={true}
                numberOfLines={8}
                maxChar={250}
                component={CustomPlainInput}
                onChange={onChangeCallback}
                keyboardType="default"
                validateForm={validateForm}
                submitted={submitted}
                styles={inputStyles}
              />
              <Field
                key="skills"
                name="skill"
                label="Skills"
                defaultValue={skills}
                spellCheck={true}
                component={CustomPlainInput}
                onChange={onChangeCallback}
                keyboardType="default"
                validateForm={validateForm}
                submitted={submitted}
                styles={inputStyles}
              />
              <Field
                key="hourlyRate"
                name="hourlyRate"
                label="Hourly Rate"
                defaultValue={hourlyRate}
                component={CustomPlainInput}
                onChange={onChangeCallback}
                keyboardType="numeric"
                validateForm={validateForm}
                submitted={submitted}
                styles={inputStyles}
              />
            </>
          )}
          <FullWidthButton
            colorScheme={colorScheme}
            loadingIndicator={isLoading}
            onPressCallback={handleSubmit}
            style={styles.submitButton}
            text={"Save Changes"}
            textStyle={styles.submitButtonText}
            disabled={!isValid}
            isOnboarding
          />
        </>
      )}
    </Formik>
  )
}

const styles = StyleSheet.create({
  submitButton: {
    ...Buttons.bar.transparent_dark,
  },
  submitButtonText: {
    ...Buttons.barText.transparent_dark,
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
    alignSelf: "center",
    height: 22, // inspect element in expo to see how much pixels it needs
    paddingHorizontal: Sizing.x8,
    marginTop: Sizing.x5,
    justifyContent: "center",
    backgroundColor: Colors.danger.s300,
    borderRadius: Outlines.borderRadius.base,
  },
  error: {
    ...Forms.inputLabel.error,
    color: Colors.primary.neutral,
  },
})

const inputStyles_light = StyleSheet.create({
  label: {
    ...Forms.inputLabel.primary_light,
  },
})

const inputStyles_dark = StyleSheet.create({
  label: {
    ...Forms.inputLabel.primary_dark,
  },
})
