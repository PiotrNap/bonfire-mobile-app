import * as React from "react"
import { StyleProp, StyleSheet, View } from "react-native"

import { Formik, Field } from "formik"
import Filter from "bad-words"

import { accountValidationScheme } from "lib/validators"
import { Sizing } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useUpdateAccountInfo } from "lib/hooks/useUpdateAccountInfo"
import { appContext } from "contexts/contextApi"
import { CustomInput } from "./CustomInput"
import { showInappropriateContentModal } from "lib/modalAlertsHelpers"
import { inputStyles, formStyleDark, formStyleLight } from "../../styles/forms"
import { ProfileContext } from "contexts/profileContext"
import { showErrorToast, showSuccessToast } from "lib/helpers"

interface Props {
  onInfoHasChanged: (val: boolean) => void
  onUpdateResponse: (val: { msg: string; status: number }) => void
  userInfo: any
}

export const UpdateAccountForm = ({ onUpdateResponse, userInfo }: Props) => {
  const { isLoading, setIsLoading, updateAccountInfo } = useUpdateAccountInfo()
  const { colorScheme } = appContext()
  const { setUsername, setBio, setProfession, setJobTitle, setSkills, setHourlyRateAda } =
    React.useContext(ProfileContext)
  const [submitted, setSubmitted] = React.useState<boolean>(false)

  const isLightMode = colorScheme === "light"
  const handleSubmit = async (newValues: any) => {
    const bw = new Filter()
    const words = Object.values(newValues).join(" ")
    if (bw.isProfane(words)) return showInappropriateContentModal()

    var hasChanged: boolean = false
    for (let k of Object.keys(newValues)) {
      if (newValues[k] !== userInfo[k]) hasChanged = true
    }
    if (!hasChanged) return

    setIsLoading(true)
    try {
      const res = await updateAccountInfo(newValues, userInfo.id)
      res && onUpdateResponse(newValues)

      const { username, bio, profession, jobTitle, skills, hourlyRateAda } = newValues
      setUsername(username)
      setBio(bio)
      setProfession(profession)
      setJobTitle(jobTitle)
      setSkills(skills)
      setHourlyRateAda(hourlyRateAda)
      showSuccessToast("Success", "Your profile got updated")
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
    }
  }
  let formStyles: StyleProp<any>
  if (isLightMode) {
    formStyles = Object.assign({}, inputStyles, styles, formStyleLight)
  } else {
    formStyles = Object.assign({}, inputStyles, styles, formStyleDark)
  }
  const { hourlyRate, ..._userInfo } = userInfo

  return (
    <Formik
      validationSchema={accountValidationScheme()}
      validateOnChange={submitted}
      validateOnBlur={submitted}
      initialValues={{
        ..._userInfo,
        ada: hourlyRate?.ada || 0,
        gimbals: hourlyRate?.gimbals || 0,
      }}
      onSubmit={handleSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="username"
            name="username"
            label="Username"
            defaultValue={userInfo.username}
            component={CustomInput}
            keyboardType="default"
            textContentType="username"
            autoCompleteType="username"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
          />
          <Field
            key="profession"
            name="profession"
            label="Profession"
            spellCheck={true}
            defaultValue={userInfo.profession}
            component={CustomInput}
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
            keyboardType="default"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
          />
          <Field
            id="hourlyRate-ada"
            key="hourlyRateAda"
            name="hourlyRateAda"
            label="Hourly Rate (ADA)"
            defaultValue={String(userInfo.hourlyRateAda) ?? "0"}
            component={CustomInput}
            keyboardType="numeric"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
          />
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
