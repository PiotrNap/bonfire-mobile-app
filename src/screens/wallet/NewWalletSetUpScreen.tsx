import * as React from "react"
import { View, StyleSheet, ScrollView } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { generateKeyPair } from "lib/tweetnacl"
import Keychain, { getSupportedBiometryType } from "react-native-keychain"

import { Users } from "Api/Users"
import { WalletStackParamList } from "common/types/navigationTypes"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { WalletSetUpForm } from "components/forms/WalletSetUpForm"
import { Layout } from "components/layouts/basicLayout"
import { SmallModal } from "components/modals/SmallModal"
import { BodyText } from "components/rnWrappers/bodyText"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { setToEncryptedStorage } from "lib/encryptedStorage"
import { Wallet } from "lib/wallet"
import { WalletSetUpFormValues } from "lib/wallet/types"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { applyOpacity } from "../../styles/colors"
import { encryptWithPassword } from "lib/helpers"

type Props = StackScreenProps<WalletStackParamList, "New Wallet Set Up">

export const NewWalletSetUp = ({ route, navigation }: Props) => {
  const { colorScheme, textContent } = appContext()
  const { id } = React.useContext(ProfileContext)
  const [biometricsAccepted, setBiometricsAccepted] =
    React.useState<boolean>(false)
  const [isModalVisible, setIsModalVisible] = React.useState<boolean>(false)
  const [biometryType, setBiometryType] =
    React.useState<Keychain.BIOMETRY_TYPE | null>(null)
  const { params } = route
  const isLightMode = colorScheme === "light"

  React.useEffect(() => {
    ;(async () => {
      const type = await getSupportedBiometryType()
      setBiometryType(type)
    })()
  }, [])

  const onCheckBoxPress = () => setBiometricsAccepted((prev) => !prev)
  const onBackNavigation = () => navigation.goBack()
  const closeRiskAckModal = () => {
    setIsModalVisible((prev) => !prev)
  }
  const createWallet = async (walletForm: WalletSetUpFormValues) => {
    if (!walletForm) throw new Error(`Missing new wallet setup form values`)
    const { name, password } = walletForm

    const { mnemonic, createWalletSetupType } = route.params
    if (!mnemonic) throw new Error(`Missing mnemonic phrase`)
    if (!createWalletSetupType) throw new Error(`Missing wallet setup type`)

    // if (params.isNewWalletCreation) {
    //   navigation.navigate("Mnemonic Preview", walletFormValues)
    // }

    // -- create wallet --
    // 1.0 generate wallet address and secret key based on mnemonic passed as param
    // 1.1 store wallets' base address and wallet type(!) on a device & update User record in DB
    //
    // 1. no-mnemonic
    //   1.1. encrypt mnemonic with password and store it on device (use 'encrypt_with_password')
    //   1.2. (IF biometric enabled) create encryption key, use this as a replacement for password
    //      and store it behind biometric auth-guard
    // 2. without-mnemonic
    //   2.1. (...)
    //

    setIsModalVisible(false)
    // - send base address to our back end (to know where to fetch tx from, have insight to wallet history ic of a dispute,
    //   and to track tx of each user)
    // - store keys to encrypted storage, encrypted with the password
    // - save the name and base address unencrypted (to display on wallet screen, fetch all tx's)
    try {
      // means user comes from "creat wallet"
      if (createWalletSetupType) {
        const wallet = new Wallet()
        const walletCredentials = await wallet.init(mnemonic)
        if (!walletCredentials)
          throw new Error(`Unable to initialize a new wallet`)
        const { baseAddress, rootKey } = walletCredentials

        await Users.updateUser({ walletBaseAddress: baseAddress }, id)
        await wallet.encryptAndStoreOnDevice(
          rootKey,
          password,
          "wallet-root-key"
        )

        // Password Encrypted Mnemonic
        const passwordEncryptedMnemonic = await encryptWithPassword(
          mnemonic,
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

        if (
          biometricsAccepted &&
          createWalletSetupType === "without-mnemonic"
        ) {
          const { secretKey } = generateKeyPair()
          if (!secretKey) throw new Error(`Missing encryption key for mnemonic`)
          // Encryption Key
          const base64SecretKey = Buffer.from(secretKey).toString("base64")
          await Keychain.setGenericPassword("encryptionKey", base64SecretKey, {
            accessControl: Keychain.ACCESS_CONTROL.BIOMETRY_ANY,
            service: "@Bonfire:encryptionKey",
          })
          // Secret Key Encrypted Mnemonic
          const secretKeyEncryptedMnemonic = await encryptWithPassword(
            base64SecretKey,
            mnemonic
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

        name && (await setToEncryptedStorage("wallet-name", name))
        await setToEncryptedStorage("wallet-base-address", params.baseAddress)

        if (createWalletSetupType === "without-mnemonic") {
          navigation.navigate("Success", {
            baseAddress,
            isNewWalletCreation: true,
            navigationScreen: "Wallet Main",
            bodyText:
              "Wallet created successfully. You can make your first deposit and start browsing for events.",
          })
        } else navigation.navigate("Mnemonic Preview", { baseAddress })
      } else {
        // if user is importing mnemonic we have wallet already initialized...
        // store keys with password (& biometric)
      }

      // navigation.navigate("Wallet Main")
    } catch (e) {
      //@TODO do a better error handling
      console.error(e)
    }
  }

  return (
    <>
      <Layout scrollable backNavigationIcon backNavigationCb={onBackNavigation}>
        <View style={styles.header}>
          <HeaderText
            colorScheme={colorScheme}
            customStyles={{ marginBottom: Sizing.x10 }}>
            {textContent.wallet.common.wallet_set_up.header}
          </HeaderText>
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            {textContent.wallet.common.wallet_set_up.body}{" "}
            {route.params?.createWalletSetupType === "without-mnemonic" &&
              textContent.wallet.common.wallet_set_up.body_add}
          </SubHeaderText>
        </View>
        <View style={styles.form}>
          <WalletSetUpForm
            biometryType={biometryType}
            onCheckBoxPress={onCheckBoxPress}
            checkboxAccepted={biometricsAccepted}
            onSubmitCallback={createWallet}
          />
        </View>
      </Layout>
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
              {!params.isNewWalletCreation
                ? textContent.wallet.risk_acknowledgement.header
                : textContent.wallet.create_wallet.mnemonic_info_modal.header}
            </SubHeaderText>
            <ScrollView style={styles.modalScrollContent}>
              {!params.isNewWalletCreation ? (
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
      </SmallModal>
    </>
  )
}

const styles = StyleSheet.create({
  header: {
    width: "90%",
    marginVertical: Sizing.x15,
  },
  form: {
    width: "90%",
    flex: 1,
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
