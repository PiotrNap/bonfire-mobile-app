import * as React from "react"
import { View, StyleSheet } from "react-native"

import { FullWidthButton } from "components/buttons/fullWidthButton"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext, walletContext } from "contexts/contextApi"
import { Colors, Sizing } from "styles/index"
import { validateMnemonic, wordlists } from "bip39"
import { Wallet } from "lib/wallet"
import { Users } from "Api/Users"
import { ProfileContext } from "contexts/profileContext"
import { UserBaseDTO } from "common/interfaces/profileInterface"
import { Checkbox } from "components/forms/Checkbox"
import { showErrorToast } from "lib/helpers"

const MAX_MNEMONIC_LENGTH = 12
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

//TODO  DO NOT use this mnemonic in production
// const DEV_MNEMONIC = {
// "0": "expand",
// "1": "good",
// "2": "gun",
// "3": "morning",
// "4": "wall",
// "5": "assault",
// "6": "heart",
// "7": "punch",
// "8": "access",
// "9": "magic",
// "10": "spoon",
// "11": "tag",
// }
const DEV_MNEMONIC = {
  "0": "top",
  "1": "polar",
  "2": "minute",
  "3": "special",
  "4": "rival",
  "5": "adjust",
  "6": "burger",
  "7": "wasp",
  "8": "session",
  "9": "dinosaur",
  "10": "conduct",
  "11": "cabbage",
}

export const MnemonicInsertScreen = ({ pagerRef, prop }: any) => {
  const [words, _] = React.useState<Set<string>>(new Set(wordlists.english))
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [content, setContent] = React.useState<any>(false)
  //TODO change this to empty object '{}'
  const [tempMnemonic, setTempMnemonic] = React.useState<{}>(DEV_MNEMONIC)
  const [currMnemonicLength, setCurrentMnemonicLength] = React.useState<{}>({})
  const [inputErrors, setInputErrors] = React.useState<{
    [key: string]: boolean
  }>(MNEMONIC_ERRORS)
  const [hasError, setHasError] = React.useState<boolean>(false)
  const [nonExistingUserBaseAddr, setNonExistingUserBaseAddr] = React.useState<string>("")
  const [acceptedStoreOfflineCheckbox, setAccepteStoreOfflinedCheckbox] =
    React.useState<boolean>(false)
  const { mnemonic, setWalletKeys, setIsOfflineMnemonic, setMnemonic } = walletContext()
  const { setUsername } = React.useContext(ProfileContext)
  const isConfirmScreen = pagerRef.current?.state?.isConfirmScreen
  const mnemonicPresent = mnemonic && Object.keys(mnemonic).length > 1

  React.useEffect(() => {
    if (typeof __DEV__ === "boolean" && __DEV__) {
      setMnemonic(DEV_MNEMONIC)
    }

    if (mnemonicPresent) {
      let _tempMnemonic = Object.assign({}, mnemonic)
      emptyFields.forEach((f) => (_tempMnemonic[f] = ""))
      setTempMnemonic(_tempMnemonic)
      setContent(textContent.confirm_mnemonic)
    } else setContent(textContent.import_mnemonic)
  }, [mnemonicPresent, mnemonic])

  const isDisabledInput = (idx: number) => {
    if (mnemonicPresent) {
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
      tempMnemonicLength !== 12 ||
      (isConfirmScreen && !mnemonic) ||
      (isConfirmScreen &&
        mnemonicPresent &&
        !Object.keys(mnemonic).every(
          (key) => key in tempMnemonic && mnemonic[key] === tempMnemonic[key]
        ))
    )
  }, [currMnemonicLength, hasError])

  const onConfirmPress = async () => {
    setIsLoading(true)

    try {
      let phrase = Object.values(prop === "sign-in" ? tempMnemonic : mnemonic).join(" ")
      if (!phrase.length) return

      const walletKeys = await new Wallet().init(phrase)
      if (!walletKeys || !Object.values(walletKeys).length) throw new Error()

      let userBaseAddr = walletKeys.baseAddress || ""
      let userCred

      if (nonExistingUserBaseAddr === userBaseAddr)
        throw new Error("Please provide a different mnemonic")
      console.log(walletKeys)
      setWalletKeys(walletKeys)

      if (prop === "sign-in") {
        userCred = await Users.checkIfUserExistsForPublicKey(walletKeys.accountPubKeyHex)

        if (!userCred) {
          setNonExistingUserBaseAddr(userBaseAddr)
          throw new Error("No user found for given mnemonic.")
        }

        setMnemonic(tempMnemonic)
        setIsOfflineMnemonic(acceptedStoreOfflineCheckbox)
        setUsername(userCred?.username)
        setWalletKeys(walletKeys)
        pagerRef.current.setPage(1)
      } else pagerRef.current.setPage(3)
    } catch (e) {
      showErrorToast(e)
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

  //@TODO revert this
  // const emptyFields = [2, 7, 11]
  const emptyFields: any = []

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
  }
  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <HeaderText customStyles={{ marginBottom: Sizing.x10 }} colorScheme="dark">
          {content.header}
        </HeaderText>
        <SubHeaderText colors={[Colors.primary.neutral]}>{content.body}</SubHeaderText>
      </View>

      <View style={styles.main}>{renderTextInputs()}</View>
      <View style={styles.footerWrapper}>
        {prop === "sign-in" && (
          <View style={styles.messageWrapper}>
            <Checkbox
              tag="store-offline"
              colorMode="dark"
              onCheckBoxPress={onCheckBoxPress}
              acceptedCheckbox={acceptedStoreOfflineCheckbox}>
              I would like to store a copy on my device, and access it later through my
              user profile.
            </Checkbox>
          </View>
        )}

        <FullWidthButton
          text="Next"
          //@TODO revert this
          // disabled={isDisabledButton()}
          buttonType="transparent"
          colorScheme="dark"
          onPressCallback={onConfirmPress}
          loadingIndicator={isLoading}
        />
      </View>
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
    width: "90%",
    // height: "100%",
    alignItems: "center",
    justifyContent: "space-between",
  },
  header: {
    width: "100%",
    marginVertical: Sizing.x15,
  },
  main: {
    // flex: 1,
    // alignItems: "center",
    // marginVertical: "auto",
  },
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
    width: "100%",
    marginLeft: "auto",
    marginRight: "auto",
    marginTop: Sizing.x5,
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
