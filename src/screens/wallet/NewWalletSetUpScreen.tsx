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
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { applyOpacity } from "../../styles/colors"
import { encryptWithPassword, showErrorToast, startChallengeSequence } from "lib/helpers"
import { PasswordSetUpForm } from "components/forms/PasswordSetUpForm"
import { useNavigation } from "@react-navigation/native"

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
      const { password, password_confirm } = walletForm
      if (password !== password_confirm)
        throw new Error(`Passwords do not match. Please correct any errors.`)

      if (!rootKeyHex) throw new Error(`Missing wallets root key`)

      // - store keys to encrypted storage, encrypted with the password
      try {
        await setToEncryptedStorage("wallet-base-address", baseAddress)
        // if this works we need to change type passed as first param in other instances
        const passwordEncryptedRootKey = await encryptWithPassword(rootKeyHex, password)
        if (!passwordEncryptedRootKey)
          return showErrorToast({ message: "Unable to encrypt keys with password" })

        if (!passwordEncryptedRootKey)
          throw new Error(`Unable to encrypt root-key with password`)
        await Keychain.setGenericPassword(
          "passwordEncryptedRootKey",
          passwordEncryptedRootKey,
          {
            accessControl: Keychain.ACCESS_CONTROL.APPLICATION_PASSWORD,
            service: "@Bonfire:passwordEncryptedRootKey",
          }
        )

        /**
         * User opted-in to store mnemonic offline on the device
         */
        if (isOfflineMnemonic) {
          const mnemonicPhrase = Object.values(mnemonic).join(" ")
          // return console.log(mnemonic, password)
          const passwordEncryptedMnemonic = await encryptWithPassword(
            mnemonicPhrase,
            password
          )
          if (!passwordEncryptedMnemonic)
            throw new Error(`Unable to encrypt mnemonic with password`)
          await Keychain.setGenericPassword(
            "passwordEncryptedMnemonic",
            passwordEncryptedMnemonic,
            {
              accessControl: Keychain.ACCESS_CONTROL.BIOMETRY_ANY,
              service: "@Bonfire:passwordEncryptedMnemonic",
            }
          )

          /**
           * User choose biometry as an alternative to password
           */
          if (biometricsAccepted) {
            const { secretKey } = generateKeyPair()
            if (!secretKey) throw new Error(`Missing encryption key for mnemonic`)
            // Encryption Key
            const base64SecretKey = Buffer.from(secretKey).toString("base64")
            await Keychain.setGenericPassword("secretKey", base64SecretKey, {
              accessControl: Keychain.ACCESS_CONTROL.BIOMETRY_ANY,
              service: "@Bonfire:secretKey",
            })
            // Secret Key Encrypted Mnemonic
            const secretKeyEncryptedMnemonic = await encryptWithPassword(
              mnemonicPhrase,
              base64SecretKey
            )
            if (!secretKeyEncryptedMnemonic)
              throw new Error(`Unable to encrypt mnemonic encryption key`)
            await Keychain.setGenericPassword(
              "secretKeyEncryptedMnemonic",
              secretKeyEncryptedMnemonic,
              {
                accessControl: Keychain.ACCESS_CONTROL.BIOMETRY_ANY,
                service: "@Bonfire:secretKeyEncryptedMnemonic",
              }
            )
          }
        }

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
      {/*
      <SmallModal
        modalVisible={isModalVisible}
        onCloseModal={closeRiskAckModal}
        onRequestClose={closeRiskAckModal}
        transparent={true}>
        <View style={styles.modalView}>
          <View
            style={[
              styles.modalContent,
              {
                backgroundColor: isLightMode
                  ? Colors.primary.neutral
                  : Colors.neutral.s600,
              },
            ]}>
            <SubHeaderText
              customStyle={{ ...Typography.fontWeight.semibold }}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              {false
                ? textContent.wallet.risk_acknowledgement.header
                : textContent.wallet.create_wallet.mnemonic_info_modal.header}
            </SubHeaderText>
            <ScrollView style={styles.modalScrollContent}>
              {!false ? (
                textContent.wallet.risk_acknowledgement.body_items.map(
                  (item) => (
                    <View style={styles.modalItemWrapper}>
                      <BodyText>{item.text}</BodyText>
                    </View>
                  )
                )
              ) : (
                <View style={styles.modalItemWrapper}>
                  <BodyText>
                    {textContent.wallet.create_wallet.mnemonic_info_modal.body}
                  </BodyText>
                </View>
              )}
            </ScrollView>
            <View style={styles.modalButtonsWrapper}>
              <FullWidthButton
                text="I Understand"
                style={styles.modalButton}
                textStyle={{ ...Typography.fontSize.x25 }}
                colorScheme={colorScheme}
                onPressCallback={createWallet}
              />
              <FullWidthButton
                text="Cancel"
                style={styles.modalButton}
                textStyle={{ ...Typography.fontSize.x25 }}
                colorScheme={colorScheme}
                onPressCallback={() => setIsModalVisible(false)}
              />
            </View>
          </View>
        </View>
      </SmallModal> */}
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
