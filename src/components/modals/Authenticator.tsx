import React, { useEffect, useState } from "react"
import { BigSlideModal } from "components/modals/BigSlideModal"
import { PasswordForm } from "components/forms/PasswordForm"
import { showErrorToast } from "lib/helpers"
import {
  retrieveAccountKeyFromStorage,
  retrieveMnemonicPhraseFromStorage,
} from "lib/wallet/storage"
import { Keyboard, StyleSheet } from "react-native"
import { appContext } from "contexts/contextApi"

type Props = {
  showAuthenticator: boolean
  authRequestType: "mnemonic" | "account-key"
  onAuthenticatedCb: (secret: string | void) => Promise<void> // secret is the value retrieved from encrypted storage
  onHideAuthenticatorCb: () => void
}

export const Authenticator = ({
  onAuthenticatedCb,
  onHideAuthenticatorCb,
  showAuthenticator,
  authRequestType,
}: Props) => {
  const [authModalVisible, setAuthModalVisible] = React.useState<boolean>(false)
  const [passwordPromptModalVisible, setPasswordPromptModalVisible] =
    React.useState<boolean>(false)
  const { deviceTopInsent, biometryType } = appContext()
  const [keyboardVisible, setKeyboardVisible] = useState(false)
  const [keyboardHeight, setKeyboardHeight] = useState(0)

  useEffect(() => {
    const keyboardDidShowListener = Keyboard.addListener("keyboardDidShow", (event) => {
      setKeyboardVisible(true)
      setKeyboardHeight(event.endCoordinates.height) // Capture keyboard height
    })
    const keyboardDidHideListener = Keyboard.addListener("keyboardDidHide", () => {
      setKeyboardVisible(false)
      setKeyboardHeight(0) // Reset keyboard height when hidden
    })

    // Clean up listeners on component unmount
    return () => {
      keyboardDidShowListener.remove()
      keyboardDidHideListener.remove()
    }
  }, [])

  React.useEffect(() => {
    if (showAuthenticator) {
      setAuthModalVisible(showAuthenticator)
      setPasswordPromptModalVisible(false)
    } else {
      setAuthModalVisible(false)
      setPasswordPromptModalVisible(false)
    }
  }, [showAuthenticator])

  const showPasswordPromptModal = () => {
    setAuthModalVisible(false)
    setPasswordPromptModalVisible(true)
  }
  const startAuthentication = async ({
    password,
  }: {
    password?: string
  }): Promise<void> => {
    let res: string | void = ""
    try {
      if (password) {
        if (authRequestType === "mnemonic") {
          res = await retrieveMnemonicPhraseFromStorage("password", password)
        } else {
          res = await retrieveAccountKeyFromStorage("password", password)
        }
      } else {
        if (authRequestType === "mnemonic") {
          res = await retrieveMnemonicPhraseFromStorage("device")
        } else {
          res = await retrieveAccountKeyFromStorage("device")
        }
      }

      await onAuthenticatedCb(res)
    } catch (e) {
      showErrorToast({ error: e, topOffset: deviceTopInsent })
    } finally {
      res = ""
      password = ""
      onHideAuthenticatorCb()
    }
  }

  return (
    <>
      {authModalVisible && biometryType !== null && (
        <BigSlideModal
          header="Choose method of authentication"
          isVisible={true}
          hideModal={onHideAuthenticatorCb}
          buttonTitle="Password"
          secondButtonTitle="Biometric"
          buttonCb={showPasswordPromptModal}
          secondButtonCb={startAuthentication}
          customStyles={styles.authModal}
        />
      )}
      {(passwordPromptModalVisible || !biometryType) && (
        <BigSlideModal
          isVisible={true}
          hideModal={onHideAuthenticatorCb}
          keyboardHeight={keyboardHeight}
          keyboardVisible={keyboardVisible}
          customStyles={styles.authModal}>
          <PasswordForm onSubmitCallback={startAuthentication} />
        </BigSlideModal>
      )}
    </>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    padding: 20,
    justifyContent: "center",
  },
  input: {
    borderWidth: 1,
    borderColor: "#ccc",
    padding: 10,
    marginBottom: 20,
    borderRadius: 5,
  },
  authModal: {
    height: "auto",
  },
  passwordModal: {
    height: "60%",
  },
})
