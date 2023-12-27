import * as React from "react"
import { View, StyleSheet, Text } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, walletContext } from "contexts/contextApi"
import { BodyText } from "components/rnWrappers/bodyText"
import { monospace } from "../../styles/typography"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { ProfileContext } from "contexts/profileContext"

export const ImportMnemonicConfirmation = ({ pagerRef, prop }: any) => {
  const { textContent } = appContext()
  const { addresses } = walletContext()
  const { username } = React.useContext(ProfileContext)
  const [_walletChecksum] = React.useState<string>("")

  const onNextPress = () => {
    pagerRef.current.setPage(2)
  }

  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <HeaderText customStyles={{ marginBottom: Sizing.x10 }}>
          {textContent.wallet.import_wallet.address_confirmation.header}
        </HeaderText>
        <View>
          <SubHeaderText colors={[Colors.primary.neutral]}>
            {textContent.wallet.import_wallet.address_confirmation.body}
          </SubHeaderText>
        </View>
      </View>
      {addresses.mainnet && (
        <View style={styles.addressWrapper}>
          <BodyText customColorScheme="light" customStyle={{ ...monospace.base }}>
            {addresses.mainnet}
          </BodyText>
        </View>
      )}
      {username && (
        <View style={styles.usernameWrapper}>
          <SubHeaderText customStyle={Typography.fontWeight.semibold}>
            Username:{" "}
          </SubHeaderText>
          <BodyText customColorScheme="dark" customStyle={{ ...monospace.base }}>
            {username}
          </BodyText>
        </View>
      )}
      <View style={styles.buttonWrapper}>
        <FullWidthButton
          text="Next"
          buttonType="transparent"
          colorScheme="dark"
          onPressCallback={onNextPress}
          loadingIndicator={false}
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
    justifyContent: "space-between",
  },
  header: {
    width: "100%",
    marginVertical: Sizing.x15,
  },
  bulletPointsWrapper: {
    color: Colors.primary.neutral,
  },
  bullet: {
    width: 10,
    textAlign: "center",
  },
  bulletPoint: {
    flexDirection: "row",
    width: "90%",
    padding: Sizing.x5,
    margin: Sizing.x5,
  },
  bulletText: {},
  addressWrapper: {
    width: "85%",
    marginTop: "auto",
    padding: Sizing.x10,
    backgroundColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    borderColor: Colors.primary.s600,
  },
  usernameWrapper: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "flex-start",
    marginTop: Sizing.x10,
    marginBottom: "auto",
  },
  buttonWrapper: {
    width: "100%",
  },
})
