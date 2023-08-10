import * as React from "react"
import { View, StyleSheet } from "react-native"

import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import Filter from "bad-words"

import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { Typography, Colors, Sizing } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { ProfileContext } from "contexts/profileContext"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"
import { HourlyRate } from "common/interfaces/newEventInterface"
import { HeaderText } from "components/rnWrappers/headerText"
import { Field, Formik } from "formik"
import {
  accountValidationScheme,
  hourlyRateValidationScheme,
} from "lib/validators"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"
import { appContext } from "contexts/contextApi"
import { CustomInput } from "components/forms/CustomInput"
import { SlideDownModal } from "components/modals/SlideDownModal"

export interface UserDetailScreenProps {}

export const UserDetailsScreen = ({ pagerRef }: any) => {
  const {
    setProfession,
    setJobTitle,
    setBio,
    setSkills,
    setHourlyRate,
    profession,
    jobTitle,
    bio,
    hourlyRate,
    skills,
  } = React.useContext(ProfileContext)
  const [_profession, _setProfession] = React.useState<string>("")
  const [_jobTitle, _setJobTitle] = React.useState<string>("")
  const [_bio, _setBio] = React.useState<string>("")
  const [_hourlyRate, _setHourlyRate] = React.useState<HourlyRate>({
    ada: 0,
    gimbals: 0,
  })
  const [_skills, _setSkills] = React.useState<string>("")
  const [_username, _setUsername] = React.useState<string>("")
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [modalState, setModalState] = React.useState<any>({
    type: "safety-warning",
    visible: false,
  })
  const [formValues, setFormValues] = React.useState<any>(null)
  const formStyles = Object.assign({}, inputStyles, formStyleDark)

  const onSubmit = (values: any) => {
    const badWords = new Filter().isProfane(Object.values(values).join(" "))
    if (badWords) return showInappropriateContentModal()

    // store the values in context or pass as a route param, don't create account yet

    setFormValues(values)
    return setModalState({ type: "safety-warning", visible: true })
  }
  const onModalConfirm = () => {
    pagerRef.current.state = formValues
    pagerRef.current.state.page = 1
    pagerRef.current.setPage(1)
  }

  return (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={false}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={{ width: "90%", marginVertical: Sizing.x15 }}>
      <View style={styles.header}>
        <HeaderText>Tell us a little bit about you</HeaderText>
      </View>
      <View style={styles.formContainer}>
        <Formik
          validationSchema={accountValidationScheme()}
          initialValues={{
            username: "",
          }}
          onSubmit={onSubmit}>
          {({ handleSubmit, isValid, validateForm }) => (
            <>
              {/* we have to include 'labelStyle' due to StyleSheet inconsistency */}
              <Field
                component={CustomInput}
                label="Username *"
                key="username"
                name="username"
                labelStyle={formStyleDark.label}
                defaultValue="@CryptoPunk"
                styles={formStyles}
                placeholder="@cryptoPunk"
                placeholderTextColor={inputStyles.placeholderText.color}
                validateForm={validateForm}
                submitted={submitted}
                onCustomChange={validateForm}
              />
              <Field
                component={CustomInput}
                label="Profession"
                key="profession"
                name="profession"
                labelStyle={formStyleDark.label}
                placeholder="Doctor, therapist, developer..."
                defaultValue={profession}
                styles={formStyles}
                placeholderTextColor={inputStyles.placeholderText.color}
                validateForm={validateForm}
                submitted={submitted}
                onCustomChange={validateForm}
              />
              <Field
                component={CustomInput}
                label="Job Title"
                key="jobTitle"
                name="jobTitle"
                labelStyle={formStyleDark.label}
                placeholder="Full Stack Engineer, Sr Business..."
                textContentType="jobTitle"
                defaultValue={jobTitle}
                styles={formStyles}
                placeholderTextColor={inputStyles.placeholderText.color}
                validateForm={validateForm}
                submitted={submitted}
                onCustomChange={validateForm}
              />
              {/* when handling events with multiline, use ref._lastNativeText */}
              <Field
                component={CustomInput}
                label="About Yourself"
                key="bio"
                name="bio"
                labelStyle={formStyleDark.label}
                multiline={true}
                numberOfLines={8}
                maxChar={250}
                defaultValue={bio}
                placeholder="Passionate in helping others draw business goals and needs..."
                styles={formStyles}
                placeholderTextColor={inputStyles.placeholderText.color}
                validateForm={validateForm}
                submitted={submitted}
                onCustomChange={validateForm}
              />

              <Field
                component={CustomInput}
                label="Skills"
                key="Skills"
                name="skills"
                labelStyle={formStyleDark.label}
                placeholder="Organized, Motivated, Critical Th..."
                defaultValue={skills}
                styles={formStyles}
                placeholderTextColor={inputStyles.placeholderText.color}
                validateForm={validateForm}
                submitted={submitted}
                onCustomChange={validateForm}
              />
              <Field
                component={CustomInput}
                key="ada"
                name="hourlyRateAda"
                label="Hourly Rate (ADA)"
                keyboardType="numeric"
                submitted={submitted}
                validateForm={validateForm}
                placeholder="35 ₳ an hour"
                placeholderTextColor={inputStyles.placeholderText.color}
                styles={formStyles}
                onCustomChange={validateForm}
              />
              <FullWidthButton
                onPressCallback={handleSubmit}
                text="Next"
                buttonType="transparent"
                disabled={!isValid}
                colorScheme="dark"
              />
            </>
          )}
        </Formik>
      </View>
      <SlideDownModal
        modalType={modalState.type}
        setModalState={setModalState}
        isVisibleModal={modalState.visible}
        modalCallback={onModalConfirm}
      />
    </KeyboardAwareScrollView>
  )
}

const styles = StyleSheet.create({
  container: {
    width: "90%",
  },
  scrollView: {
    width: "100%",
    height: "100%",
    marginVertical: Sizing.x20,
  },
  header: {
    marginVertical: Sizing.x10,
  },
  headerText: {
    ...Typography.header.x65,
    color: Colors.primary.neutral,
  },
  formContainer: {
    flex: 1,
    marginVertical: Sizing.x10,
  },
})
