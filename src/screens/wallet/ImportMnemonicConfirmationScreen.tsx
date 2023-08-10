import * as React from "react"
import { View, StyleSheet } from "react-native"

import { StackScreenProps } from "@react-navigation/stack"
import { walletChecksum } from "@emurgo/cip4-js"

import { WalletStackParamList } from "common/types/navigationTypes"
import { Layout } from "components/layouts/basicLayout"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext } from "contexts/contextApi"
import { BodyText } from "components/rnWrappers/bodyText"
import { monospace } from "../../styles/typography"
import { FullWidthButton } from "components/buttons/fullWidthButton"

type Props = StackScreenProps<
  WalletStackParamList,
  "Import Mnemonic Confirmation"
>

export const ImportMnemonicConfirmation = ({ navigation, route }: Props) => {
  const { colorScheme, textContent } = appContext()
  const { params } = route
  const [_walletChecksum, setWalletChecksum] = React.useState<string>("")

  React.useEffect(() => {
    const checkSum = walletChecksum(params.accountPubKey)
    setWalletChecksum(checkSum.TextPart)
  }, [])

  const onBackNavigation = () => navigation.goBack()
  const onConfirmPress = () => {
    navigation.navigate("New Wallet Set Up", params)
  }

  return (
    <Layout backNavigationIcon backNavigationCb={onBackNavigation}>
      <View style={styles.header}>
        <HeaderText
          customStyles={{ marginBottom: Sizing.x10 }}
          colorScheme={colorScheme}>
          {textContent.wallet.import_wallet.address_confirmation.header}
        </HeaderText>
        <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
          {textContent.wallet.import_wallet.address_confirmation.body}
        </SubHeaderText>
      </View>
      {params.baseAddress ? (
        <>
          <View style={styles.addressWrapper}>
            <BodyText customStyle={{ ...monospace.base }}>
              {params.baseAddress}
            </BodyText>
          </View>
          {!!_walletChecksum && (
            <View style={styles.checksumWrapper}>
              <SubHeaderText
                colors={[Colors.primary.s800, Colors.primary.neutral]}
                customStyle={Typography.fontWeight.semibold}>
                Checksum:{" "}
              </SubHeaderText>
              <BodyText customStyle={{ ...monospace.base }}>
                {_walletChecksum}
              </BodyText>
            </View>
          )}
        </>
      ) : (
        <></>
      )}
      <FullWidthButton
        colorScheme={colorScheme}
        text="Confirm"
        onPressCallback={onConfirmPress}
        style={{ width: "90%", marginBottom: Sizing.x15 }}
      />
    </Layout>
  )
}

const styles = StyleSheet.create({
  header: {
    alignSelf: "center",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  addressWrapper: {
    width: "85%",
    marginTop: "auto",
    padding: Sizing.x10,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    borderColor: Colors.primary.s600,
  },
  checksumWrapper: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "flex-start",
    marginTop: Sizing.x10,
    marginBottom: "auto",
  },
})
