import * as React from "react"
import { View, StyleSheet } from "react-native"

import { generateKeyPair } from "lib/tweetnacl"
import Keychain, { getSupportedBiometryType } from "react-native-keychain"

import { Users } from "Api/Users"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext, walletContext } from "contexts/contextApi"
import { setToEncryptedStorage } from "lib/encryptedStorage"
import { PasswordSetUpFormValues } from "lib/wallet/types"
import { Colors, Outlines, Sizing } from "styles/index"
import { applyOpacity } from "../../styles/colors"
import { showErrorToast, startChallengeSequence } from "lib/helpers"
import { PasswordSetUpForm } from "components/forms/PasswordSetUpForm"
import { useNavigation } from "@react-navigation/native"
import { addKeysToStorage } from "lib/wallet/storage"
import { Bip32PrivateKey, bytesToHex } from "@hyperionbt/helios"
import AsyncStorage from "@react-native-async-storage/async-storage"

export const NewWalletSetUp = ({ pagerRef, prop }: any) => {
  const [biometricsAccepted, setBiometricsAccepted] = React.useState<boolean>(false)
  const [biometryType, setBiometryType] = React.useState<Keychain.BIOMETRY_TYPE | null>(
    null
  )
  const {
    mnemonic,
    rootKeyHex,
    isOfflineMnemonic,
    baseAddress,
    accountKeyHex,
    accountPubKeyHex,
    resetSecrets,
  } = walletContext()
  const { setPageIndex } = appContext()
  const navigation = useNavigation()

  React.useEffect(() => {
    ;(async () => {
      const type = await getSupportedBiometryType()
      console.log("biometry type ?", type)
      setBiometryType(type)
    })()
  }, [])

  const onCheckBoxPress = () => setBiometricsAccepted((prev) => !prev)
  const createWallet = React.useCallback(
    async (walletForm: PasswordSetUpFormValues) => {
      if (!walletForm) throw new Error(`Missing new wallet setup form values`)
      let { password, password_confirm } = walletForm
      if (password !== password_confirm)
        throw new Error(`Passwords do not match. Please correct any errors.`)

      if (!rootKeyHex) throw new Error(`Missing wallets root key`)
      // for not having to derive it each time from root key...
      const privKeyArray = Array.from(Buffer.from(accountKeyHex, "hex"))
      const baseAddressKey = bytesToHex(
        new Bip32PrivateKey(privKeyArray).derive(0).derive(0).bytes
      )

      try {
        // store root key + base address (+ mnemonic) securely
        await addKeysToStorage(
          isOfflineMnemonic,
          biometricsAccepted,
          password,
          rootKeyHex,
          baseAddress,
          baseAddressKey,
          isOfflineMnemonic ? mnemonic : undefined
        )
        // whether to show the mnemonic preview option in user settings
        await AsyncStorage.setItem("isOfflineMnemonic", String(isOfflineMnemonic))

        password = ""
        password_confirm = ""

        if (prop === "sign-in") {
          const keyPair = generateKeyPair()
          let deviceSecKey = Buffer.from(keyPair?.secretKey).toString("base64")
          let devicePubKey = Buffer.from(keyPair?.publicKey).toString("base64")

          // throws an error when no user exists for a given walletPublicKey
          let registerRes = await Users.registerDevice({
            walletPublicKey: accountPubKeyHex,
            devicePubKey,
          })
          if (!registerRes) throw new Error()

          let authResponseDTO = await startChallengeSequence(
            deviceSecKey,
            registerRes.deviceID,
            registerRes.id
          )

          console.log(`
        adding to encrypted storage:
            deviceID: ${registerRes.deviceID}
            authCred: ${authResponseDTO}
            devicePubKey: ${devicePubKey}
            deviceSecKey: ${deviceSecKey}
            `)
          await setToEncryptedStorage("device-id", registerRes.deviceID)
          await setToEncryptedStorage("auth-credentials", authResponseDTO)
          await setToEncryptedStorage("device-privKey", deviceSecKey)
          await setToEncryptedStorage("device-pubKey", devicePubKey)

          // clean up secret keys
          pagerRef.current.state = null
          resetSecrets()
          devicePubKey = ""
          deviceSecKey = ""
          //@ts-ignore
          navigation.navigate("Navigation Screens")
        }
        setPageIndex(4)
        pagerRef.current.setPage(4)
      } catch (e) {
        showErrorToast(e)
      }
    },
    [mnemonic, rootKeyHex, isOfflineMnemonic, baseAddress, biometricsAccepted]
  )

  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <HeaderText customStyles={{ marginBottom: Sizing.x10 }}>
          Create Spending Password
        </HeaderText>
        <SubHeaderText>
          To gain access to your wallet's funds and calendar schedulings, you'll need to
          set up a password.
        </SubHeaderText>
      </View>
      <View style={styles.form}>
        <PasswordSetUpForm
          pageType={prop}
          biometryType={biometryType}
          onCheckBoxPress={onCheckBoxPress}
          checkboxAccepted={biometricsAccepted}
          onSubmitCallback={createWallet}
        />
      </View>
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    width: "90%",
    height: "100%",
    alignItems: "center",
  },
  header: {
    flex: 1,
    width: "100%",
    marginTop: Sizing.x15,
  },
  form: {
    flex: 2,
    width: "100%",
    justifyContent: "space-between",
  },
  /** Modal background **/
  modalView: {
    flex: 1,
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: applyOpacity(Colors.neutral.s600, 0.4),
  },
  /** Modal Window **/
  modalContent: {
    padding: Sizing.x12,
    borderRadius: Outlines.borderRadius.base,
    width: "80%",
    height: "80%",
  },
  modalScrollContent: {},
  modalItemWrapper: {
    marginVertical: Sizing.x5,
    paddingTop: Sizing.x15,
    paddingBottom: Sizing.x5,
  },
  modalButtonsWrapper: {
    margin: Sizing.x10,
  },
  modalButton: {
    width: "80%",
    alignSelf: "center",
    paddingVertical: Sizing.x8,
  },
})
