import { StackScreenProps } from "@react-navigation/stack"
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
import * as React from "react"
import { View, StyleSheet, Pressable, Text } from "react-native"
import { ScrollView } from "react-native-gesture-handler"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { applyOpacity } from "../../styles/colors"

type Props = StackScreenProps<WalletStackParamList, "New Wallet Set Up">

export const NewWalletSetUp = ({ route, navigation }: Props) => {
  const { colorScheme, textContent } = appContext()
  const { id } = React.useContext(ProfileContext)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [walletFormValues, setWalletFormValues] =
    React.useState<WalletSetUpFormValues | null>(null)
  const [isModalVisible, setIsModalVisible] = React.useState<boolean>(false)
  const [checkboxSelected, setCheckboxSelected] = React.useState<any>({
    0: false,
    1: false,
  })
  const { params } = route
  const isLightMode = colorScheme === "light"

  const onBackNavigation = () => navigation.goBack()
  const onSubmitCallback = async (walletForm: WalletSetUpFormValues) => {
    setIsLoading(true)
    setIsModalVisible(true)
    setWalletFormValues(walletForm)
  }
  const closeRiskAckModal = () => {
    setIsModalVisible((prev) => !prev)
  }
  const createWallet = async () => {
    if (!walletFormValues) return

    setIsLoading(true)
    setIsModalVisible(false)
    // - send base address to our back end (to know where to fetch tx from, have insight to wallet history ic of dispute, track tx of users)
    // - store keys to encrypted storage, encrypted with the password
    // - save the name and base address unencrypted (to display on wallet screen, fetch all tx's)
    try {
      // update user entity
      await Users.updateUser({ walletBaseAddress: params.baseAddress }, id)

      const wallet = new Wallet()
      const { password, name } = walletFormValues

      await wallet.encryptAndStoreOnDevice(
        params.rootKey,
        password,
        "wallet-root-key"
      )
      await wallet.encryptAndStoreOnDevice(
        Object.values(params.mnemonics).join(" "),
        password,
        "mnemonic"
      )
      await setToEncryptedStorage("wallet-name", name)
      await setToEncryptedStorage("wallet-base-address", params.baseAddress)

      setIsLoading(false)
      navigation.navigate("Wallet")
    } catch (e) {
      console.error(e)
      setIsLoading(false)
    }
  }

  return (
    <>
      <Layout backNavigationIcon backNavigationCb={onBackNavigation}>
        <View style={styles.header}>
          <HeaderText
            colorScheme={colorScheme}
            customStyles={{ marginBottom: Sizing.x10 }}>
            Set Up a New Wallet
          </HeaderText>
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            Give your wallet a name and provide a unique spending password. This
            password will be required to spend your assets and gain access to
            the recovery phrase. Make sure to make a backup copy.
          </SubHeaderText>
        </View>
        <View style={styles.form}>
          <WalletSetUpForm onSubmitCallback={onSubmitCallback} />
        </View>
      </Layout>
      <SmallModal
        modalVisible={isModalVisible}
        onCloseModal={closeRiskAckModal}
        onRequestClose={closeRiskAckModal}
        transparent={true}>
        <View style={styles.modalView}>
          <ScrollView
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
              {textContent.wallet.risk_acknowledgement.header}
            </SubHeaderText>
            {textContent.wallet.risk_acknowledgement.body_items.map((item) => (
              <View style={styles.modalItemWrapper}>
                <BodyText>{item.text}</BodyText>
              </View>
            ))}
            <View style={{ marginBottom: "auto" }}>
              <FullWidthButton
                text="I Understand"
                style={styles.modalButton}
                textStyle={{ ...Typography.fontSize.x25 }}
                colorScheme={colorScheme}
                onPressCallback={() => createWallet()}
              />
              <FullWidthButton
                text="Cancel"
                style={styles.modalButton}
                textStyle={{ ...Typography.fontSize.x25 }}
                colorScheme={colorScheme}
                onPressCallback={() => setIsModalVisible(false)}
              />
            </View>
          </ScrollView>
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
    maxHeight: "70%",
  },
  modalItemWrapper: {
    marginVertical: Sizing.x5,
    padding: Sizing.x5,
  },
  modalButton: {
    width: "80%",
    alignSelf: "center",
    paddingVertical: Sizing.x8,
  },
})
