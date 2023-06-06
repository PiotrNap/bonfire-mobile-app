import * as React from "react"
import { StyleSheet, StyleProp } from "react-native"

import { Field, Formik } from "formik"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { appContext } from "contexts/contextApi"
import { walletSetUpValidationScheme } from "lib/validators"
import { Sizing } from "styles/index"
import { CustomInput } from "./CustomInput"
import { CustomPasswordInput } from "./CustomPasswordInput"
import { inputStyles, formStyleDark, formStyleLight } from "../../styles/forms"
import { WalletSetUpFormValues } from "lib/wallet/types"

export interface Props {
  onSubmitCallback: (values: WalletSetUpFormValues) => void
}

export const WalletSetUpForm = ({ onSubmitCallback }: Props) => {
  const { colorScheme } = appContext()
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [hidePassword, setHidePassword] = React.useState<boolean>(true)

  const isLightMode = colorScheme === "light"
  const onChangeCallback = () => {}

  let formStyles: StyleProp<any>
  if (isLightMode) {
    formStyles = Object.assign({}, inputStyles, styles, formStyleLight)
  } else {
    formStyles = Object.assign({}, inputStyles, styles, formStyleDark)
  }
  return (
    <Formik
      validationSchema={walletSetUpValidationScheme()}
      initialValues={{
        name: "",
        password: "",
        password_confirm: "",
      }}
      validateForm={async (values: any) => console.log(values)}
      onSubmit={onSubmitCallback}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="name"
            name="name"
            label="Name"
            component={CustomInput}
            onChange={onChangeCallback}
            defaultValue={""}
            keyboardType="default"
            textContentType="name"
            autoCompleteType="name"
            validateForm={validateForm}
            submitted={submitted}
            styles={formStyles}
            placeholder={"My Wallet"}
          />
          <Field
            key="password"
            name="password"
            label="Password"
            defaultValue={""}
            component={CustomPasswordInput}
            onChange={onChangeCallback}
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
            onChange={onChangeCallback}
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

          <FullWidthButton
            text="Confirm"
            disabled={!isValid}
            style={{ marginTop: "auto", marginBottom: Sizing.x15 }}
            colorScheme={colorScheme}
            onPressCallback={handleSubmit}
            loadingIndicator={isLoading}
          />
        </>
      )}
    </Formik>
  )
}

const styles = StyleSheet.create({})
