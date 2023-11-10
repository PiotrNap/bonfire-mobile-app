import * as React from "react"
import { StyleSheet, StyleProp, View } from "react-native"

import { Field, Formik } from "formik"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { passwordSetUpValidationScheme, passwordValidationSchema } from "lib/validators"
import { Sizing } from "styles/index"
import { CustomPasswordInput } from "./CustomPasswordInput"
import { inputStyles, formStyleDark, formStyleLight } from "../../styles/forms"
import { appContext } from "contexts/contextApi"

export interface Props {
  onSubmitCallback: ({ password }: { password: string }) => Promise<void>
  styles?: StyleProp<View>
}

export const PasswordForm = ({ onSubmitCallback, styles }: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [hidePassword, setHidePassword] = React.useState<boolean>(true)

  const onSubmit = async (arg: any) => {
    setIsLoading(true)
    setSubmitted(true)
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
      validationSchema={passwordValidationSchema()}
      initialValues={{
        password: "",
      }}
      onSubmit={onSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="password"
            name="password"
            label="Spending Password"
            component={CustomPasswordInput}
            keyboardType="default"
            textContentType="password"
            autoCompleteType="password"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
            passwordHideCb={() => setHidePassword((prev) => !prev)}
            hidePassword={hidePassword}
            showIcon
          />

          <FullWidthButton
            text={"Confirm"}
            disabled={!isValid}
            buttonType="filled"
            colorScheme={colorScheme}
            onPressCallback={handleSubmit}
            loadingIndicator={isLoading}
          />
        </>
      )}
    </Formik>
  )
}
