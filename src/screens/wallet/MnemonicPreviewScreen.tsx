import * as React from "react"
import { View, StyleSheet, Pressable, Text } from "react-native"

import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext, walletContext } from "contexts/contextApi"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { BodyText } from "components/rnWrappers/bodyText"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { Checkbox } from "components/forms/Checkbox"
import { useWalletInit } from "lib/hooks/useWalletInit"
import { monospace } from "../../styles/typography"
import { CheckIcon } from "assets/icons"
import Clipboard from "@react-native-clipboard/clipboard"

export const MnemonicPreview = ({ pagerRef }: any) => {
  const { textContent } = appContext()
  const [phraseCopied, setPhraseCopied] = React.useState<boolean>(false)
  const [acceptedBackupCopyCheckbox, setAcceptedBackupCopyCheckbox] =
    React.useState<boolean>(false)
  const [acceptedStoreOfflineCheckbox, setAccepteStoreOfflinedCheckbox] =
    React.useState<boolean>(false)
  const { setMnemonic, setIsOfflineMnemonic } = walletContext()
  const {
    setMnemonicString,
    mnemonicString: mnemonic,
    generateMnemonic,
  } = useWalletInit()

  React.useEffect(() => {
    let mnemonic = generateMnemonic()
    let seed = {}
    mnemonic.split(" ").forEach((w: string, idx: number) => (seed[idx] = w))
    setMnemonic(seed)
    setMnemonicString(mnemonic)
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
    setIsOfflineMnemonic(acceptedStoreOfflineCheckbox)

    pagerRef.current.state = {}
    pagerRef.current.state.isConfirmScreen = true
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
        <View style={styles.bodyWrapper}>
          <View style={styles.body}>
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
          <Pressable onPress={onPhraseCopy} style={Buttons.applyOpacity(styles.copyBtn)}>
            {phraseCopied ? (
              <CheckIcon stroke={Colors.primary.s800} style={styles.icon} />
            ) : (
              <Text style={styles.buttonText}>Copy</Text>
            )}
          </Pressable>
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
            I would like to store a copy on my device, and access it later through my user
            profile.
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
    padding: Sizing.x10,
    marginTop: Sizing.x25,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    borderColor: Colors.primary.s600,
    backgroundColor: Colors.primary.neutral,
  },
  bodyWrapper: {
    width: "85%",
    alignItems: "center",
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
  copyBtn: {
    alignItems: "center",
    justifyContent: "center",
    flexDirection: "row",
    marginHorizontal: "auto",
    marginTop: Sizing.x10,
    width: Sizing.x100,
    height: Sizing.x45,
    backgroundColor: Colors.primary.s200,
    borderWidth: Outlines.borderWidth.thick,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.max,
  },
  buttonText: {
    ...Typography.header.x25,
    color: Colors.primary.s800,
    marginHorizontal: Sizing.x1,
    textAlign: "center",
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
    marginHorizontal: Sizing.x1,
  },
})
