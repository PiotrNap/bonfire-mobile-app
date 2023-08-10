import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"

import { WalletStackParamList } from "common/types/navigationTypes"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { Layout } from "components/layouts/basicLayout"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext, walletContext } from "contexts/contextApi"
import { RECOVERY_PHRASE_LENGTHS } from "lib/wallet/config"
import { Colors, Sizing } from "styles/index"
import { getRandomKey } from "lib/utils"
import { SlideTopModal } from "components/modals/SlideTopModal"
import { ErrorIcon } from "assets/icons"
import { useWalletInit } from "lib/hooks/useWalletInit"
import { validateMnemonic, wordlists } from "bip39"

// type Props = StackScreenProps<WalletStackParamList, "Import Mnemonic">
const MAX_MNEMONIC_LENGTH = 12

export const ImportMnemonicScreen = ({ pagerRef }: any) => {
  const [isErrorModalVisible, setIsErrorModalVisible] =
    React.useState<boolean>(false)
  const [words, _] = React.useState<Set<string>>(new Set(wordlists.english))
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [content, setContent] = React.useState<any>(false)
  const [tempMnemonic, setTempMnemonic] = React.useState<{}>({})
  const { error, init, validMnemonic } = useWalletInit()
  const { mnemonic, setMnemonic } = walletContext()

  React.useEffect(() => {
    if (mnemonic) {
      setContent(textContent.confirm_mnemonic)
    } else setContent(textContent.import_mnemonic)
  }, [mnemonic])

  const isDisabledInput = (idx: number) => {
    if (mnemonic) {
      return !emptyFields.includes(idx)
    } else {
      if (idx === 1) return false
      if (idx !== 1 && !mnemonic) return true
      if (mnemonic) {
        if (!mnemonic[idx - 1]) return true
        const emptyFieldIdx =
          Object.values(mnemonic).findIndex((val) => !val) + 1
        if (!emptyFieldIdx) return false

        return idx > emptyFieldIdx
      }
      return false
    }
  }
  const isDisabledButton =
    !mnemonic ||
    Object.values(mnemonic).length !== 12 ||
    Object.values(mnemonic)
  // !RECOVERY_PHRASE_LENGTHS.includes(
  //   Object.values(mnemonic).filter((m) => !!m).length
  // )

  const onConfirmPress = async () => {
    // setIsLoading(true)

    let valid = validMnemonic()
    debugger
    return
    try {
      const wallet = await init()
      setIsLoading(false)
      navigation.navigate("Import Mnemonic Confirmation", {
        ...wallet,
        mnemonic,
      })
      setIsErrorModalVisible(false)
    } catch (e) {
      setIsErrorModalVisible(true)
      setIsLoading(false)
    }
  }
  const onInputBoxUpdate = (value: string, idx: number) => {
    setTempMnemonic({ ...tempMnemonic, [String(idx)]: value })
  }

  const errorModalHideCallback = () => setIsErrorModalVisible(false)
  const validateMnemonicInputField = (val: string): boolean => {
    return words.has(val)
  }
  const emptyFields = [2, 7, 11]

  const renderTextInputs = () => {
    const inputs = []
    let rowInputsSum = 0
    for (let i = 0; i < MAX_MNEMONIC_LENGTH / 3; i++) {
      inputs.push(
        <View style={styles.inputRow} key={i}>
          <View style={styles.inputRowItem}>
            <CustomPlainInput
              defaultValue={
                mnemonic && !emptyFields.includes(rowInputsSum)
                  ? mnemonic[rowInputsSum]
                  : ""
              }
              onEndEditingCallback={onInputBoxUpdate}
              isDisabled={isDisabledInput(rowInputsSum)}
              idx={rowInputsSum}
              validate={validateMnemonicInputField}
            />
          </View>
          <View style={styles.inputRowItem}>
            <CustomPlainInput
              defaultValue={
                mnemonic && !emptyFields.includes(1 + rowInputsSum)
                  ? mnemonic[1 + rowInputsSum]
                  : ""
              }
              onEndEditingCallback={onInputBoxUpdate}
              isDisabled={isDisabledInput(1 + rowInputsSum)}
              idx={1 + rowInputsSum}
              validate={validateMnemonicInputField}
            />
          </View>
          <View style={styles.inputRowItem}>
            <CustomPlainInput
              defaultValue={
                mnemonic && !emptyFields.includes(2 + rowInputsSum)
                  ? mnemonic[2 + rowInputsSum]
                  : ""
              }
              onEndEditingCallback={onInputBoxUpdate}
              isDisabled={isDisabledInput(2 + rowInputsSum)}
              idx={2 + rowInputsSum}
              validate={validateMnemonicInputField}
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
      <View style={styles.container}>
        <View style={styles.header}>
          <HeaderText
            customStyles={{ marginBottom: Sizing.x10 }}
            colorScheme="dark">
            {content.header}
          </HeaderText>
          <SubHeaderText colors={[Colors.primary.neutral]}>
            {content.body}
          </SubHeaderText>
        </View>

        <View style={styles.main}>{renderTextInputs()}</View>

        <FullWidthButton
          text="Next"
          disabled={isDisabledButton}
          buttonType="transparent"
          colorScheme="dark"
          onPressCallback={onConfirmPress}
          loadingIndicator={isLoading}
        />
      </View>
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
  container: {
    width: "90%",
    height: "100%",
    alignItems: "center",
    justifyContent: "space-between",
  },
  header: {
    width: "100%",
    marginVertical: Sizing.x15,
  },
  main: {
    width: "100%",
    alignItems: "center",
    marginVertical: "auto",
  },
  inputRow: {
    flexDirection: "row",
    width: "100%",
    justifyContent: "space-between",
    marginBottom: Sizing.x8,
  },
  inputRowItem: {
    flexDirection: "row",
    width: Sizing.x90,
    alignItems: "center",
    justifyContent: "flex-end",
  },
  recoveryPhraseInputNumber: {
    marginRight: Sizing.x2,
  },
})

const textContent = {
  import_mnemonic: {
    header: "Import an Existing Wallet",
    body: "Please insert each word of your recovery phrase into boxes below. Words are case sensitive and should be lower case.",
    button: "",
  },
  confirm_mnemonic: {
    header: "Confirm Recovery Phrase",
    body: "To help you make sure you've written down your recovery phrase correctly. Please fill each missing word in the boxes below.",
    button: "Next",
  },
}
