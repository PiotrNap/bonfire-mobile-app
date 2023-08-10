import * as React from "react"
import { View, StyleSheet, Pressable } from "react-native"

import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext, walletContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { BodyText } from "components/rnWrappers/bodyText"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { Checkbox } from "components/forms/Checkbox"
import { useWalletInit } from "lib/hooks/useWalletInit"
import { monospace } from "../../styles/typography"
import { CheckIcon, DuplicateIcon } from "assets/icons"
import Clipboard from "@react-native-clipboard/clipboard"

export const MnemonicPreview = ({ pagerRef }: any) => {
  const { textContent } = appContext()
  const [phraseCopied, setPhraseCopied] = React.useState<boolean>(false)
  const [acceptedBackupCopyCheckbox, setAcceptedBackupCopyCheckbox] =
    React.useState<boolean>(false)
  const [acceptedStoreOfflineCheckbox, setAccepteStoreOfflinedCheckbox] =
    React.useState<boolean>(false)
  const { setMnemonic } = walletContext()
  const {
    setMnemonicString,
    mnemonicString: mnemonic,
    generateMnemonic,
  } = useWalletInit()

  React.useEffect(() => {
    //TODO reverse this
    // let mnemonic = generateMnemonic()
    let test =
      "girl awesome blast actual cry alter coral mimic visa avocado swim repeat"
    let seed = {}
    test.split(" ").forEach((w: string, idx: number) => (seed[idx] = w))
    setMnemonic(seed)
    // setMnemonic(mnemonic)
    setMnemonicString(test)
  }, [])

  const onCheckBoxPress = (tag?: string) => {
    if (!tag) return

    tag === "store-offline"
      ? setAccepteStoreOfflinedCheckbox((prev) => !prev)
      : setAcceptedBackupCopyCheckbox((prev) => !prev)
  }
  const onPhraseCopy = () => {
    if (phraseCopied || !mnemonic) return

    Clipboard.setString(mnemonic)
    setPhraseCopied(true)
    setTimeout(() => setPhraseCopied(false), 3000)
  }
  const onConfirmPress = () => {
    pagerRef.current.state.offlineMnemonic = acceptedStoreOfflineCheckbox
    pagerRef.current.setPage(2)
  }

  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <HeaderText customStyles={{ marginBottom: Sizing.x10 }}>
          {textContent.wallet.create_wallet.mnemonic_preview.header}
        </HeaderText>
        <SubHeaderText>
          {textContent.wallet.create_wallet.mnemonic_preview.body}
        </SubHeaderText>
      </View>
      {!!mnemonic && (
        <View style={styles.body}>
          <Pressable
            hitSlop={10}
            onPress={onPhraseCopy}
            style={styles.iconWrapper}>
            {phraseCopied ? (
              <CheckIcon stroke={Colors.primary.s800} />
            ) : (
              <DuplicateIcon stroke={Colors.primary.s800} />
            )}
          </Pressable>
          <BodyText
            customStyle={{
              ...monospace.base,
              color: Colors.primary.s800,
              ...Typography.fontSize.x35,
              ...Typography.lineHeight.x45,
            }}>
            {mnemonic}
          </BodyText>
        </View>
      )}
      <View style={styles.footerWrapper}>
        <View style={styles.messageWrapper}>
          <Checkbox
            tag="back-up-copy"
            colorMode="dark"
            onCheckBoxPress={onCheckBoxPress}
            acceptedCheckbox={acceptedBackupCopyCheckbox}>
            I have made a backup copy.
          </Checkbox>
          <Checkbox
            tag="store-offline"
            colorMode="dark"
            onCheckBoxPress={onCheckBoxPress}
            acceptedCheckbox={acceptedStoreOfflineCheckbox}>
            I would like to store a copy on my device, and access it later
            through my user profile.
          </Checkbox>
        </View>
        <FullWidthButton
          text="Next"
          disabled={!acceptedBackupCopyCheckbox}
          buttonType="transparent"
          colorScheme="dark"
          onPressCallback={onConfirmPress}
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
    width: "100%",
    marginVertical: Sizing.x15,
  },
  body: {
    marginVertical: "auto",
    width: "85%",
    padding: Sizing.x10,
    marginTop: Sizing.x25,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    borderColor: Colors.primary.s600,
    backgroundColor: Colors.primary.neutral,
  },
  messageWrapper: {
    width: "100%",
    marginLeft: "auto",
    marginRight: "auto",
    marginTop: Sizing.x5,
  },
  footerWrapper: {
    marginTop: "auto",
    width: "100%",
  },
  iconWrapper: {
    zIndex: 10,
    width: Sizing.x40,
    height: Sizing.x40,
    padding: Sizing.x3,
    margin: Sizing.x5,
    bottom: 0,
    right: 0,
    position: "absolute",
    borderWidth: Outlines.borderWidth.thick,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.max,
  },
})
