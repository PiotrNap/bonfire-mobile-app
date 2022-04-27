import * as React from "react"
import { View, StyleSheet, Text, TextInput } from "react-native"

import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import Filter from "bad-words"

import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { Typography, Colors, Sizing, Forms } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { ProfileContext } from "contexts/profileContext"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"

export interface UserDetailScreenProps {}

export const UserDetailsScreen = ({ pagerRef }: any) => {
  const { setProfession, setJobTitle, setBio, setSkills, setHourlyRate } =
    React.useContext(ProfileContext)
  const [_profession, _setProfession] = React.useState<string>("")
  const [_jobTitle, _setJobTitle] = React.useState<string>("")
  const [_bio, _setBio] = React.useState<string>("")
  const [_hourlyRate, _setHourlyRate] = React.useState<number>(0)
  const [_skills, _setSkills] = React.useState<string>("")

  const submitBioState = () => {
    const badWords = new Filter().isProfane(
      [_profession, _jobTitle, _bio, _skills].join(" ")
    )
    if (badWords) return showInappropriateContentModal()

    setProfession(_profession.trim())
    setJobTitle(_jobTitle.trim())
    setBio(_bio.trim())
    setHourlyRate(_hourlyRate)
    setSkills(_skills.trim())
    pagerRef.current.setPage(1)
  }

  return (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={false}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={{ width: "90%", marginVertical: Sizing.x15 }}>
      <View style={styles.header}>
        <Text style={styles.headerText}>
          Tell us a little bit{"\n"}about you
        </Text>
      </View>
      <View style={styles.formContainer}>
        {/* we have to include 'labelStyle' due to StyleSheet inconsistency */}
        <CustomPlainInput
          label="Profession"
          labelStyle={inputStyles.label}
          placeholder="Doctor, therapist, developer..."
          styles={inputStyles}
          onEndEditingCallback={(val) => _setProfession(val)}
        />
        <CustomPlainInput
          label="Job Title"
          labelStyle={inputStyles.label}
          placeholder="Full Stack Engineer, Sr Business..."
          styles={inputStyles}
          textContentType="jobTitle"
          onEndEditingCallback={(val) => _setJobTitle(val)}
        />
        {/* when handling events with multiline, use ref._lastNativeText */}
        <CustomPlainInput
          label="About Yourself"
          labelStyle={inputStyles.label}
          multiline={true}
          numberOfLines={8}
          maxChar={250}
          placeholder="Passionate in helping others draw business goals and needs..."
          styles={inputStyles}
          onEndEditingCallback={(val) => _setBio(val)}
        />
        <CustomPlainInput
          label="Skills"
          labelStyle={inputStyles.label}
          placeholder="Organized, Motivated, Critical Th..."
          styles={inputStyles}
          onEndEditingCallback={(val) => _setSkills(val)}
        />
      </View>
      <View style={inputStyles.inputContainer}>
        <View style={inputStyles.labelContainer}>
          <Text style={inputStyles.label}>Hourly Rate (ADA)</Text>
        </View>
        <View style={inputStyles.textInputWrapper}>
          <TextInput
            style={inputStyles.input}
            keyboardType="numeric"
            textContentType="none"
            placeholder="35 ₳ an hour"
            placeholderTextColor={inputStyles.placeholderText.color}
            onChangeText={(val) => _setHourlyRate(Number(val))}
          />
        </View>
      </View>
      <FullWidthButton
        onPressCallback={submitBioState}
        text="Next"
        buttonType="transparent"
        colorScheme="dark"
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
/**
 * styles passed as prop to CustomPlainInput
 */
const inputStyles = StyleSheet.create({
  inputContainer: {
    flex: 1,
    width: "100%",
    alignItems: "center",
    marginBottom: Sizing.x10,
  },
  labelContainer: {
    width: "100%",
  },
  label: {
    ...Forms.inputLabel.primary_dark,
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
})
