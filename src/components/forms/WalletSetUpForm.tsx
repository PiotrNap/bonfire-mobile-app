import * as React from "react"
import { StyleSheet, StyleProp, View } from "react-native"

import { Field, Formik } from "formik"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { appContext } from "contexts/contextApi"
import { walletSetUpValidationScheme } from "lib/validators"
import { Sizing } from "styles/index"
import { CustomInput } from "./CustomInput"
import { CustomPasswordInput } from "./CustomPasswordInput"
import { inputStyles, formStyleDark, formStyleLight } from "../../styles/forms"
import { WalletSetUpFormValues } from "lib/wallet/types"
import { Checkbox } from "components/forms/Checkbox"

export interface Props {
  onSubmitCallback: (values: WalletSetUpFormValues) => Promise<void>
  checkboxAccepted: boolean
  biometryType: any
  onCheckBoxPress: () => void
}

export const WalletSetUpForm = ({
  onSubmitCallback,
  checkboxAccepted,
  onCheckBoxPress,
  biometryType,
}: Props) => {
  const { colorScheme } = appContext()
  const [submitted, setSubmitted] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [hidePassword, setHidePassword] = React.useState<boolean>(true)

  const isLightMode = colorScheme === "light"
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
      validationSchema={walletSetUpValidationScheme()}
      initialValues={{
        //@TODO remove these default values
        name: "my-wallet",
        password: "qwert12345",
        password_confirm: "qwert12345",
      }}
      validateForm={async (values: any) => console.log(values)}
      onSubmit={onSubmit}>
      {({ handleSubmit, isValid, validateForm }) => (
        <>
          <Field
            key="name"
            name="name"
            label="Name"
            component={CustomInput}
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

          {biometryType && (
            <View style={styles.checkboxWrapper}>
              <Checkbox
                acceptedCheckbox={checkboxAccepted}
                onCheckBoxPress={onCheckBoxPress}>
                Enable biometric authentication
              </Checkbox>
            </View>
          )}

          <FullWidthButton
            text="Confirm"
            disabled={!isValid}
            style={{ marginBottom: Sizing.x15 }}
            colorScheme={colorScheme}
            onPressCallback={handleSubmit}
            loadingIndicator={isLoading}
          />
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
