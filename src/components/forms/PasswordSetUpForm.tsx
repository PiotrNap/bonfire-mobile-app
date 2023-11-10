import * as React from "react"
import { StyleSheet, StyleProp, View } from "react-native"

import { Field, Formik } from "formik"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { passwordSetUpValidationScheme } from "lib/validators"
import { Sizing } from "styles/index"
import { CustomPasswordInput } from "./CustomPasswordInput"
import { inputStyles, formStyleDark, formStyleLight } from "../../styles/forms"
import { PasswordSetUpFormValues } from "lib/wallet/types"
import { Checkbox } from "components/forms/Checkbox"

export interface Props {
  onSubmitCallback: (values: PasswordSetUpFormValues) => Promise<void>
  checkboxAccepted: boolean
  biometryType: any
  pageType: "sign-in" | "sign-up"
  onCheckBoxPress: () => void
}

export const PasswordSetUpForm = ({
  onSubmitCallback,
  checkboxAccepted,
  onCheckBoxPress,
  biometryType,
  pageType,
}: Props) => {
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [hidePassword, setHidePassword] = React.useState<boolean>(true)

  const isLightMode = false
  const onSubmit = async (arg: any) => {
    setIsLoading(true)
    await onSubmitCallback(arg)
    setIsLoading(false)
  }

  let formStyles: StyleProp<any>
  if (isLightMode) {
    formStyles = Object.assign({}, inputStyles, styles, formStyleLight)
  } else {
    formStyles = Object.assign({}, inputStyles, styles, formStyleDark)
  }
  return (
    <Formik
      validationSchema={passwordSetUpValidationScheme()}
      initialValues={{
        //@TODO remove this placeholder
        password: "qwert12345",
        password_confirm: "qwert12345",
      }}
      onSubmit={onSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <View>
            <Field
              key="password"
              name="password"
              label="New Password"
              defaultValue={""}
              component={CustomPasswordInput}
              keyboardType="default"
              textContentType="newPassword"
              autoCompleteType="password"
              validateForm={validateForm}
              submitted={submitted}
              styles={formStyles}
              placeholder={"UniquePassword123"}
              passwordHideCb={() => setHidePassword((prev) => !prev)}
              hidePassword={hidePassword}
              showIcon
            />
            <Field
              key="password_confirm"
              name="password_confirm"
              label="Confirm Password"
              defaultValue={""}
              component={CustomPasswordInput}
              keyboardType="default"
              textContentType="newPassword"
              autoCompleteType="password"
              validateForm={validateForm}
              submitted={submitted}
              styles={formStyles}
              placeholder={"UniquePassword123"}
              hidePassword={hidePassword}
              isPasswordConfirmation
            />
          </View>

          <View>
            {biometryType && (
              <View style={styles.checkboxWrapper}>
                <Checkbox
                  colorMode="dark"
                  acceptedCheckbox={checkboxAccepted}
                  onCheckBoxPress={onCheckBoxPress}>
                  Enable biometric authentication
                </Checkbox>
              </View>
            )}

            <FullWidthButton
              text={pageType === "sign-up" ? "Next" : "Confirm"}
              disabled={!isValid}
              style={{ marginBottom: Sizing.x15 }}
              buttonType="transparent"
              onPressCallback={handleSubmit}
              loadingIndicator={isLoading}
            />
          </View>
        </>
      )}
    </Formik>
  )
}

const styles = StyleSheet.create({
  checkboxWrapper: {
    flexDirection: "row",
    alignItems: "center",
    marginVertical: Sizing.x5,
  },
})
