import React from "react"
import { BigSlideModal } from "components/modals/BigSlideModal"
import { PasswordForm } from "components/forms/PasswordForm"
import { showErrorToast } from "lib/helpers"
import {
  retrieveAccountKeyFromStorage,
  retrieveMnemonicPhraseFromStorage,
} from "lib/wallet/storage"
import { StyleSheet } from "react-native"
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
    const {deviceTopInsent} = appContext()

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
      showErrorToast({error: e, topOffset: deviceTopInsent})
    } finally {
      res = ""
      password = ""
      onHideAuthenticatorCb()
    }
  }
  return (
    <>
      {authModalVisible && (
        <BigSlideModal
          header="Choose method of authentication"
          isVisible={authModalVisible}
          hideModal={onHideAuthenticatorCb}
          buttonTitle="Password"
          secondButtonTitle="Biometric"
          buttonCb={showPasswordPromptModal}
          secondButtonCb={startAuthentication}
          customStyles={styles.authModal}
        />
      )}
      {passwordPromptModalVisible && (
        <BigSlideModal
          isVisible={passwordPromptModalVisible}
          hideModal={onHideAuthenticatorCb}
          customStyles={styles.authModal}>
          <PasswordForm onSubmitCallback={startAuthentication} />
        </BigSlideModal>
      )}
    </>
  )
}

const styles = StyleSheet.create({
  authModal: {
    height: "auto",
  },
  passwordModal: {
    height: "60%",
  },
})
