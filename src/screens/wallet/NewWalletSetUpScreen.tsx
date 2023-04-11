import { StackScreenProps } from "@react-navigation/stack"
import { WalletStackParamList } from "common/types/navigationTypes"
import { WalletSetUpForm } from "components/forms/WalletSetUpForm"
import { Layout } from "components/layouts/basicLayout"
import { BigSlideModal } from "components/modals/bigSlideModal"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { Wallet } from "lib/wallet"
import { WalletSetUpFormValues } from "lib/wallet/types"
import * as React from "react"
import { View, StyleSheet } from "react-native"
import { Colors, Sizing } from "styles/index"

type Props = StackScreenProps<WalletStackParamList, "New Wallet Set Up">

export const NewWalletSetUp = ({ route, navigation }: Props) => {
  const { colorScheme, textContent } = appContext()
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [isModalVisible, setIsModalVisible] = React.useState<boolean>(false)
  const { params } = route

  const onBackNavigation = () => navigation.goBack()
  const onSubmitCallback = async ({
    name,
    password,
    password_confirm,
  }: WalletSetUpFormValues) => {
    const wallet = new Wallet()
    try {
      console.log("before encryption ", params.rootKey)
      await wallet.encryptAndStoreOnDevice(
        params.rootKey,
        password,
        "wallet-root-key"
      )
      const decrypted = await wallet.retrieveAndDecryptFromDevice(
        password,
        "wallet-root-key"
      )
      console.log("after decryption ", decrypted)
    } catch (e) {
      console.error(e)
    }
  }
  const onConfirmPress = () => {}

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
})
