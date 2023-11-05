import * as React from "react"
import { View, StyleSheet } from "react-native"

import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import Filter from "bad-words"

import { Typography, Colors, Sizing } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"
import { HeaderText } from "components/rnWrappers/headerText"
import { Field, Formik } from "formik"
import { accountValidationScheme } from "lib/validators"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"
import { CustomInput } from "components/forms/CustomInput"
import { SlideDownModal } from "components/modals/SlideDownModal"
import { ProfileContext } from "contexts/profileContext"
import { Users } from "Api/Users"
import { showErrorToast } from "lib/helpers"

export interface UserDetailScreenProps {}

export const UserDetailsScreen = ({ pagerRef }: any) => {
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [modalState, setModalState] = React.useState<any>({
    type: "safety-warning",
    visible: false,
  })
  const [formValues, setFormValues] = React.useState<any>(null)
  const { setUsername, setBio, setSkills, setJobTitle, setHourlyRateAda, setProfession } =
    React.useContext(ProfileContext)
  const formStyles = Object.assign({}, inputStyles, formStyleDark)

  const onSubmit = async (values: any) => {
    try {
      const badWords = new Filter().isProfane(Object.values(values).join(" "))
      if (badWords) return showInappropriateContentModal()

      // check if chosen username is available
      const usernameFree = await Users.checkUsernameAvailability(values.username)
      if (!usernameFree) return showErrorToast({ message: "Username already taken" })

      // store the values in context or pass as a route param, don't create account yet
      setFormValues(values)
      return setModalState({ type: "safety-warning", visible: true })
    } catch (e) {
      showErrorToast(e)
    }
  }
  const onModalConfirm = () => {
    setUsername(formValues.username)
    setBio(formValues.bio)
    setProfession(formValues.profession)
    setSkills(formValues.skills)
    setJobTitle(formValues.jobTitle)
    setHourlyRateAda(formValues.hourlyRateAda)

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
