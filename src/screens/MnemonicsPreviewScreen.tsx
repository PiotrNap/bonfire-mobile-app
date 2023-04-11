import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { WalletStackParamList } from "common/types/navigationTypes"
import { Layout } from "components/layouts/basicLayout"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { generateMnemonic, Wallet } from "lib/wallet"
import { BodyText } from "components/rnWrappers/bodyText"
import { monospace } from "../styles/typography"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { Checkbox } from "components/forms/Checkbox"

type Props = StackScreenProps<WalletStackParamList, "Mnemonic Preview">

export const MnemonicsPreview = ({ route, navigation }: Props) => {
  const { colorScheme, textContent } = appContext()
  const [mnemonicPhrase, setMnemonicPhrase] = React.useState<string | null>(
    null
  )
  const [acceptedCheckbox, setAcceptedCheckbox] = React.useState<boolean>(false)
  const isLightMode = colorScheme === "light"

  React.useEffect(() => {
    let mnemonic = generateMnemonic()
    setMnemonicPhrase(mnemonic)
  }, [])
  const onCheckBoxPress = () => setAcceptedCheckbox((prev) => !prev)
  const onConfirmPress = () => {}

  return (
    <Layout backNavigationIcon backNavigationCb={navigation.goBack}>
      <View style={styles.header}>
        <HeaderText
          colorScheme={colorScheme}
          customStyles={{ marginBottom: Sizing.x10 }}>
          {textContent.wallet.create_wallet.mnemonic_preview.header}
        </HeaderText>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          {textContent.wallet.create_wallet.mnemonic_preview.body}
        </SubHeaderText>
      </View>
      {!!mnemonicPhrase && (
        <View style={styles.body}>
          <BodyText
            customStyle={{
              ...monospace.base,
              color: Colors.primary.s800,
              ...Typography.fontSize.x35,
              ...Typography.lineHeight.x45,
            }}>
            {mnemonicPhrase}
          </BodyText>
        </View>
      )}
      <View style={styles.footerWrapper}>
        <View style={styles.messageWrapper}>
          <Checkbox
            onCheckBoxPress={onCheckBoxPress}
            acceptedCheckbox={acceptedCheckbox}>
            I confirm making a backup copy of my new mnemonic phrase
          </Checkbox>
        </View>
        <FullWidthButton
          colorScheme={colorScheme}
          text="Confirm"
          disabled={!acceptedCheckbox}
          onPressCallback={onConfirmPress}
          style={{ width: "90%", marginBottom: Sizing.x15 }}
        />
      </View>
    </Layout>
  )
}

const styles = StyleSheet.create({
  header: {
    width: "90%",
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
  },
  messageWrapper: {
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    marginTop: Sizing.x5,
  },
  footerWrapper: {
    alignItems: "center",
    marginTop: "auto",
    width: "100%",
  },
})
