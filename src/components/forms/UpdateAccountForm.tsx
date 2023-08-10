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
    const { ada, gimbals, ...rest } = newValues
    const bw = new Filter()
    const words = Object.values(newValues).join(" ")
    if (bw.isProfane(words)) return showInappropriateContentModal()

    var hasChanged: boolean = false

    if (accountType === "organizer") {
      rest.hourlyRate = {
        ada: Number(newValues.ada),
        gimbals: Number(newValues.gimbals),
      }
      for (let k of Object.keys(newValues)) {
        if (newValues[k] !== userInfo[k]) hasChanged = true
      }
    } else {
      if (rest.name !== userInfo.name || rest.username !== userInfo.username)
        hasChanged = true
    }
    if (!hasChanged) return

    setIsLoading(true)
    const res = await updateAccountInfo(rest, userInfo.id)
    res && onUpdateResponse(res)
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
            key="name"
            name="name"
            label="Name"
            component={CustomInput}
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
