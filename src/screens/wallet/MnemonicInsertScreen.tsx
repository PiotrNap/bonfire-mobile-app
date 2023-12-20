import * as React from "react"
import { View, StyleSheet } from "react-native"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { walletContext } from "contexts/contextApi"
import { Colors, Sizing, Typography } from "styles/index"
import { Wallet } from "lib/wallet"
import { Users } from "Api/Users"
import { ProfileContext } from "contexts/profileContext"
import { Checkbox } from "components/forms/Checkbox"
import { showErrorToast } from "lib/helpers"
import wordlist from "bip39/src/wordlists/english.json"
import { ToggleButton } from "screens/organizer/newEvent/toggleButton"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { MnemonicInputsProvider } from "contexts/mnemonicInputsContext"

const MNEMONIC_ERRORS = {
  0: false,
  1: false,
  2: false,
  3: false,
  4: false,
  5: false,
  7: false,
  8: false,
  9: false,
  10: false,
  11: false,
}

export const MnemonicInsertScreen = ({ pagerRef, prop, pageIndex }: any) => {
  const {
    mnemonic,
    setWalletKeys,
    setIsOfflineMnemonic,
    setMnemonic,
    seedPhraseWordCount,
    setSeedPhraseWordCount,
  } = walletContext()
  const { setUsername } = React.useContext(ProfileContext)
  const [words, _] = React.useState<Set<string>>(new Set(wordlist))
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [content, setContent] = React.useState<any>(false)
  const [tempMnemonic, setTempMnemonic] = React.useState<{}>({})
  const [currMnemonicLength, setCurrentMnemonicLength] = React.useState<{}>({})
  const [selectedMnemonicLength, setSelectedMnemonicLength] =
    React.useState<number>(seedPhraseWordCount)
  const [inputErrors, setInputErrors] = React.useState<{
    [key: string]: boolean
  }>(MNEMONIC_ERRORS)
  const [hasError, setHasError] = React.useState<boolean>(false)
  const [nonExistingUserBaseAddr, setNonExistingUserBaseAddr] = React.useState<string>("")
  const [acceptedStoreOfflineCheckbox, setAccepteStoreOfflinedCheckbox] =
    React.useState<boolean>(false)
  const isConfirmScreen = pagerRef.current?.state?.isConfirmScreen
  const mnemonicPresent = mnemonic && Object.keys(mnemonic).length > 1

  React.useEffect(() => {
    if (mnemonicPresent && !prop) {
      let _tempMnemonic = Object.assign({}, mnemonic)
      emptyFields.forEach((f) => (_tempMnemonic[f] = ""))
      setTempMnemonic(_tempMnemonic)
      setContent(textContent.confirm_mnemonic)
    } else setContent(textContent.import_mnemonic)

    setSelectedMnemonicLength(seedPhraseWordCount)
  }, [mnemonicPresent, mnemonic, pageIndex, seedPhraseWordCount])

  const isDisabledInput = (idx: number) => {
    if (prop === "import-mnemonic") return false

    if (mnemonicPresent && prop !== "import-mnemonic") {
      return !emptyFields.includes(idx)
    } else {
      if (idx === 1) return false
      if (idx !== 1 && !mnemonic) return true
      if (mnemonicPresent) {
        if (!mnemonic[idx - 1]) return true
        const emptyFieldIdx = Object.values(mnemonic).findIndex((val) => !val) + 1
        if (!emptyFieldIdx) return false

        return idx > emptyFieldIdx
      }
      return false
    }
  }

  const isDisabledButton = React.useCallback(() => {
    let tempMnemonicLength = Object.values(tempMnemonic).filter((w) => w != "").length
    return (
      hasError ||
      tempMnemonicLength !== selectedMnemonicLength ||
      (isConfirmScreen && !mnemonic) ||
      (isConfirmScreen &&
        mnemonicPresent &&
        !Object.keys(mnemonic).every(
          (key) => key in tempMnemonic && mnemonic[key] === tempMnemonic[key]
        ))
    )
  }, [currMnemonicLength, hasError, selectedMnemonicLength])

  const onNextButtonPress = async () => {
    setIsLoading(true)

    try {
      let phrase = Object.values(
        prop === "sign-in" || prop === "import-mnemonic" ? tempMnemonic : mnemonic
      ).join(" ")
      if (!phrase.length) return

      const walletKeys = await new Wallet().init(phrase)
      if (!walletKeys || !Object.values(walletKeys).length) throw new Error()

      let userBaseAddr = walletKeys.testnetBaseAddress || ""
      let userCred

      if (nonExistingUserBaseAddr === userBaseAddr)
        throw new Error("Please provide a different mnemonic")

      if (!prop) {
        setWalletKeys(walletKeys)
        return pagerRef.current.setPage(3)
      }

      userCred = await Users.checkIfUserExistsForPublicKey(walletKeys.accountPubKeyHex)

      if (prop === "sign-in" && !userCred) {
        setNonExistingUserBaseAddr(userBaseAddr)
        throw new Error("No user found for a given mnemonic.")
      } else if (prop === "import-mnemonic" && userCred) {
        throw new Error("User already exist for a given mnemonic.")
      }

      setMnemonic(tempMnemonic)
      setIsOfflineMnemonic(acceptedStoreOfflineCheckbox)
      setUsername(userCred?.username || "")
      setWalletKeys(walletKeys)
      setSeedPhraseWordCount(selectedMnemonicLength)

      pagerRef.current.setPage(1)
    } catch (e) {
      showErrorToast("Please double-check each input field", "Incorrect Mnemonic")
    } finally {
      setIsLoading(false)
    }
  }
  const onInputBoxUpdate = (value: string, idx: number) => {
    value = value?.trim()
    setTempMnemonic((prev) => {
      prev[idx] = value.trim()
      setCurrentMnemonicLength(
        Object.values(prev)
          .filter((w) => w != "")
          .join(" ").length
      )

      return prev
    })
  }

  const emptyFields = [2, 7, 11]
  const validateMnemonicInputField = (val: string, idx: any): boolean => {
    val = val?.trim()
    if (isConfirmScreen && mnemonic) return mnemonic[idx] === val
    return words.has(val)
  }
  const onError = (isValid: boolean, idx: any) => {
    setInputErrors((prev) => {
      let next = prev
      next[idx] = !isValid

      let error = Object.values(next).filter((ie) => ie).length > 0
      setHasError(error)
      return next
    })
  }
  const onCheckBoxPress = (tag?: string) => {
    if (!tag) return
    if (tag === "store-offline") setAccepteStoreOfflinedCheckbox((prev) => !prev)
  }
  const changeMnemonicLength = (length: number) => {
    setSelectedMnemonicLength(length)
  }

  const renderTextInputs = React.useCallback(() => {
    const inputs = []
    let rowInputsSum = 0
    for (let i = 0; i < selectedMnemonicLength / 3; i++) {
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
              onError={onError}
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
              onError={onError}
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
              onError={onError}
            />
          </View>
        </View>
      )
      rowInputsSum += 3
    }
    return inputs
  }, [selectedMnemonicLength])
  return (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={false}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={{ width: "90%", marginVertical: Sizing.x15 }}>
      <View style={styles.header}>
        <HeaderText customStyles={{ marginBottom: Sizing.x10 }} colorScheme="dark">
          {content.header}
        </HeaderText>
        <SubHeaderText colors={[Colors.primary.neutral]}>{content.body}</SubHeaderText>
      </View>

      <View style={styles.main}>
        <SubHeaderText
          customStyle={styles.mnemonicLengthLabel}
          colors={[Colors.primary.neutral]}>
          Select Seed Phrase Length
        </SubHeaderText>
        <ToggleButton
          defaultValue={selectedMnemonicLength}
          options={["12", "15", "18", "21", "24"]}
          values={[12, 15, 18, 21, 24]}
          animationDuration={30}
          onSelect={changeMnemonicLength}
          customColorScheme={"dark"}
        />
        <MnemonicInputsProvider>{renderTextInputs()}</MnemonicInputsProvider>
      </View>
      <View style={styles.footerWrapper}>
        {prop && (
          <View style={styles.messageWrapper}>
            <Checkbox
              tag="store-offline"
              colorMode="dark"
              onCheckBoxPress={onCheckBoxPress}
              acceptedCheckbox={acceptedStoreOfflineCheckbox}>
              I would like to store a copy on my device (accessible later through my User
              Profile)
            </Checkbox>
          </View>
        )}

        <FullWidthButton
          text="Next"
          disabled={isDisabledButton()}
          buttonType="transparent"
          colorScheme="dark"
          onPressCallback={onNextButtonPress}
          loadingIndicator={isLoading}
        />
      </View>
    </KeyboardAwareScrollView>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    width: "90%",
    alignItems: "center",
    justifyContent: "space-between",
  },
  header: {
    width: "100%",
    marginVertical: Sizing.x15,
  },
  main: {},
  inputRow: {
    width: " 100%",
    flexDirection: "row",
    justifyContent: "space-between",
    marginBottom: Sizing.x8,
  },
  inputRowItem: {
    flex: 1,
    flexDirection: "row",
    marginHorizontal: Sizing.x5,
    alignItems: "center",
    justifyContent: "flex-end",
  },
  recoveryPhraseInputNumber: {
    marginRight: Sizing.x2,
  },
  footerWrapper: {
    width: "100%",
  },
  messageWrapper: {
    marginVertical: Sizing.x10,
  },
  mnemonicLengthLabel: {
    flex: 1,
    ...Typography.subHeader.x10,
    paddingLeft: Sizing.x5,
    paddingBottom: Sizing.x5,
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
    body: "To help you make sure you've written down your recovery phrase correctly, please fill in each missing word in the boxes below.",
    button: "Next",
  },
}
