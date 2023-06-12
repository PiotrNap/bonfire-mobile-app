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
import { hourlyRateValidationScheme } from "lib/validators"
import { formStyleDark, formStyleLight, inputStyles } from "../../styles/forms"
import { appContext } from "contexts/contextApi"
import { CustomInput } from "components/forms/CustomInput"

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
  const [submitted, setSubmitted] = React.useState<boolean>(false)

  const onSubmit = (hourlyRateValues: any) => {
    const badWords = new Filter().isProfane(
      [_profession, _jobTitle, _bio, _skills].join(" ")
    )
    if (badWords) return showInappropriateContentModal()
    const { ada, gimbals } = hourlyRateValues

    setHourlyRate({
      ..._hourlyRate,
      ada: Number(ada || 0),
      gimbals: Number(gimbals || 0),
    })
    setProfession(_profession.trim())
    setJobTitle(_jobTitle.trim())
    setBio(_bio.trim())
    setSkills(_skills.trim())
    pagerRef.current.setPage(1)
  }
  const formStyles = Object.assign({}, inputStyles, formStyleDark)

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
        {/* we have to include 'labelStyle' due to StyleSheet inconsistency */}
        <CustomPlainInput
          label="Profession"
          labelStyle={formStyleDark.label}
          placeholder="Doctor, therapist, developer..."
          styles={formStyleDark}
          defaultValue={profession}
          onEndEditingCallback={(val) => _setProfession(val)}
        />
        <CustomPlainInput
          label="Job Title"
          labelStyle={formStyleDark.label}
          placeholder="Full Stack Engineer, Sr Business..."
          styles={formStyleDark}
          textContentType="jobTitle"
          defaultValue={jobTitle}
          onEndEditingCallback={(val) => _setJobTitle(val)}
        />
        {/* when handling events with multiline, use ref._lastNativeText */}
        <CustomPlainInput
          label="About Yourself"
          labelStyle={formStyleDark.label}
          multiline={true}
          numberOfLines={8}
          maxChar={250}
          defaultValue={bio}
          placeholder="Passionate in helping others draw business goals and needs..."
          styles={formStyleDark}
          onEndEditingCallback={(val) => _setBio(val)}
        />
        <CustomPlainInput
          label="Skills"
          labelStyle={formStyleDark.label}
          placeholder="Organized, Motivated, Critical Th..."
          styles={formStyleDark}
          defaultValue={skills}
          onEndEditingCallback={(val) => _setSkills(val)}
        />
      </View>
      <Formik
        validationSchema={hourlyRateValidationScheme()}
        initialValues={{
          name: "",
          username: "",
        }}
        onSubmit={onSubmit}>
        {({ handleSubmit, isValid, validateForm }) => (
          <>
            <Field
              component={CustomInput}
              key="ada"
              name="ada"
              label="Hourly Rate (ADA)"
              keyboardType="numeric"
              defaultValue={String(hourlyRate?.ada || 0)}
              submitted={submitted}
              validateForm={validateForm}
              placeholder="35 ₳ an hour"
              placeholderTextColor={inputStyles.placeholderText.color}
              styles={formStyles}
              onCustomChange={validateForm}
            />
            <Field
              component={CustomInput}
              key="gimbals"
              name="gimbals"
              label="Hourly Rate (Gimbals)"
              keyboardType="numeric"
              placeholder="5000 GMBL an hour"
              defaultValue={String(hourlyRate?.gimbals || 0)}
              submitted={submitted}
              validateForm={validateForm}
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
// /**
//  * styles passed as prop to CustomPlainInput
//  */
// const inputStyles = StyleSheet.create({
//   inputContainer: {
//     flex: 1,
//     width: "100%",
//     alignItems: "center",
//     marginBottom: Sizing.x10,
//   },
//   labelContainer: {
//     width: "100%",
//   },
//   label: {
//     ...Forms.inputLabel.primary_dark,
//   },
//   textInputWrapper: {
//     width: "100%",
//     flexDirection: "row",
//     alignItems: "center",
//   },
//   input: {
//     width: "100%",
//     ...Forms.input.primary,
//   },
//   placeholderText: {
//     color: Colors.primary.s300,
//   },
// })
