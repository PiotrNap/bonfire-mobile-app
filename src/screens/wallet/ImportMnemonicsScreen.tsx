import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"

import { WalletStackParamList } from "common/types/navigationTypes"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { Layout } from "components/layouts/basicLayout"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { RECOVERY_PHRASE_LENGTHS } from "lib/wallet/config"
import { Colors, Sizing } from "styles/index"
import { getRandomKey } from "lib/utils"
import { SlideTopModal } from "components/modals/SlideTopModal"
import { ErrorIcon } from "assets/icons"
import { useWalletInit } from "lib/hooks/useWalletInit"

type Props = StackScreenProps<WalletStackParamList, "Import Mnemonics">
const MAX_MNEMONICS_LENGTH = 24

export const ImportMnemonicsScreen = ({ navigation }: Props) => {
  const [isErrorModalVisible, setIsErrorModalVisible] =
    React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const { colorScheme, textContent } = appContext()
  const { error, mnemonics, setMnemonics, init } = useWalletInit()

  const isDisabledInput = (idx: number) => {
    if (idx === 1) return false
    if (idx !== 1 && !mnemonics) return true
    if (mnemonics && !mnemonics[idx - 1]) return true
    return false
  }
  const isDisabledButton =
    !mnemonics ||
    !RECOVERY_PHRASE_LENGTHS.includes(
      Object.values(mnemonics).filter((m) => !!m).length
    )

  const onBackNavigationPress = () => navigation.navigate("Wallet")
  const onConfirmPress = async () => {
    setIsLoading(true)
    try {
      const wallet = await init()
      setIsLoading(false)
      navigation.navigate("Import Mnemonics Confirmation", {
        ...wallet,
        mnemonics,
      })
      setIsErrorModalVisible(false)
    } catch (e) {
      setIsErrorModalVisible(true)
      setIsLoading(false)
    }
  }
  const onInputBoxUpdate = (value: string, idx: number) => {
    if (!mnemonics) {
      setMnemonics({ [String(idx)]: value })
    } else setMnemonics((prev) => ({ ...prev, [String(idx)]: value }))
  }
  const errorModalHideCallback = () => setIsErrorModalVisible(false)
  const renderTextInputs = () => {
    const inputs = []
    let rowInputsSum = 0
    for (let i = 0; i < MAX_MNEMONICS_LENGTH / 3; i++) {
      inputs.push(
        <View style={styles.inputRow}>
          <View style={styles.inputRowItem}>
            <SubHeaderText
              customStyle={styles.recoveryPhraseInputNumber}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              {1 + rowInputsSum}.
            </SubHeaderText>
            <CustomPlainInput
              onEndEditingCallback={onInputBoxUpdate}
              isDisabled={isDisabledInput(1 + rowInputsSum)}
              idx={1 + rowInputsSum}
            />
          </View>
          <View style={styles.inputRowItem}>
            <SubHeaderText
              customStyle={styles.recoveryPhraseInputNumber}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              {2 + rowInputsSum}.
            </SubHeaderText>
            <CustomPlainInput
              onEndEditingCallback={onInputBoxUpdate}
              isDisabled={isDisabledInput(2 + rowInputsSum)}
              idx={2 + rowInputsSum}
            />
          </View>
          <View style={styles.inputRowItem}>
            <SubHeaderText
              customStyle={styles.recoveryPhraseInputNumber}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              {3 + rowInputsSum}.
            </SubHeaderText>
            <CustomPlainInput
              onEndEditingCallback={onInputBoxUpdate}
              isDisabled={isDisabledInput(3 + rowInputsSum)}
              idx={3 + rowInputsSum}
            />
          </View>
        </View>
      )
      rowInputsSum += 3
    }
    return inputs
  }
  return (
    <>
      <Layout
        scrollable
        backNavigationIcon
        backNavigationCb={onBackNavigationPress}>
        <View style={styles.header}>
          <HeaderText
            customStyles={{ marginBottom: Sizing.x10 }}
            colorScheme={colorScheme}>
            {textContent.wallet.import_wallet.import_mnemonics.header}
          </HeaderText>
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            {textContent.wallet.import_wallet.import_mnemonics.body}
          </SubHeaderText>
        </View>

        <View style={styles.main}>{renderTextInputs()}</View>

        <FullWidthButton
          text="Confirm"
          disabled={isDisabledButton}
          style={{ width: "90%", marginBottom: Sizing.x15 }}
          colorScheme={colorScheme}
          onPressCallback={onConfirmPress}
          loadingIndicator={isLoading}
        />
      </Layout>
      {!!error && (
        <SlideTopModal
          icon={
            <ErrorIcon
              stroke={Colors.primary.neutral}
              width={Sizing.x60}
              height={Sizing.x60}
              strokeWidth={1.5}
            />
          }
          isModalVisible={isErrorModalVisible}
          modalContent={error}
          backgroundColor={Colors.danger.s300}
          hideCallback={errorModalHideCallback}
        />
      )}
    </>
  )
}

const styles = StyleSheet.create({
  header: {
    alignSelf: "center",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  main: {
    width: "90%",
    alignItems: "center",
  },
  inputRow: {
    flexDirection: "row",
    width: "90%",
    justifyContent: "space-between",
  },
  inputRowItem: {
    flexDirection: "row",
    width: Sizing.x80,
    alignItems: "center",
    justifyContent: "flex-end",
  },
  recoveryPhraseInputNumber: {
    marginRight: Sizing.x5,
  },
})
