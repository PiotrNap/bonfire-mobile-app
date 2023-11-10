import React from "react"

import { AdaIcon, LeftArrowIcon } from "assets/icons"
import { SectionDetail } from "common/interfaces/bookingInterface"
import { ConfirmationDetail } from "components/booking"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, walletContext } from "contexts/contextApi"
import { FlatList, Pressable, SafeAreaView, StyleSheet, View } from "react-native"
import { Colors, Sizing } from "styles/index"
import { fromAssetUnit, hexToText, lovelaceToAda } from "lib/wallet/utils"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import Crypto from "crypto"
import { showErrorToast } from "lib/helpers"
import { Wallet } from "lib/wallet"
import { Authenticator } from "components/modals/Authenticator"

export function PreviewTransactionScreen({ navigation, route }: any) {
  const { colorScheme } = appContext()
  const { sendTxInfo, baseAddress, walletUtxos, setSendTxInfo } = walletContext()
  const [authenticatorVisible, setAuthenticatorVisible] = React.useState<boolean>(false)
  const isLightMode = colorScheme === "light"
  const onBackNavigationPress = () => navigation.goBack()

  const iconStyles = {
    stroke: isLightMode ? Colors.primary.s800 : Colors.primary.s200,
    strokeWidth: 1.8,
    width: Sizing.x25,
    height: Sizing.x25,
    marginRight: Sizing.x5,
  }
  const cutStringInside = (str: string | undefined) => {
    if (!str) return
    return `${str.substring(0, 15)}...${str.substring(str.length - 15, str.length)}`
  }
  const onSignAndSubmit = () => {
    if (!sendTxInfo) return showErrorToast("Missing send transaction info")
    setAuthenticatorVisible(true)
  }
  const onAuthenticated = async (accountKey?: string) => {
    if (!sendTxInfo) {
      showErrorToast("Missing send transaction info")
      accountKey = ""
      return
    }
    if (!accountKey) return showErrorToast("Something went wrong. Missing signing key.")
    try {
      await Wallet.sendRegularTransaction(
        sendTxInfo,
        baseAddress,
        walletUtxos,
        accountKey
      )

      setSendTxInfo({})
      setAuthenticatorVisible(false)
      navigation.navigate("Success", {
        headerText: "Successful transaction",
        bodyText: "Details should be visible in your wallet in just a moment.",
        navigationScreen: "Wallet Main",
      })
    } catch (e) {
      showErrorToast(e)
    } finally {
      accountKey = ""
    }
  }
  const onHideAuthenticator = () => setAuthenticatorVisible(false)

  const txDetails: any[] = [
    {
      label: "To",
      lineContent: {
        content: cutStringInside(sendTxInfo?.receiverAddress),
      },
    },
    {
      label: "ADA",
      lineContent: {
        content: lovelaceToAda(BigInt(sendTxInfo?.lovelace || 0)).toFixed(2),
        icon: <AdaIcon {...iconStyles} />,
      },
    },
    sendTxInfo?.assets?.size && {
      label: "Assets",
      lineContent: Array.from(sendTxInfo?.assets).map((asset: any) => {
        return {
          content: `${hexToText(asset[1].name)} - (${Number(asset[1].count).toFixed(2)})`,
        }
      }),
    },
  ].filter((txDetail) => txDetail)

  const renderItem = ({ item, index }: { item: SectionDetail; index: number }) => {
    if (!item.lineContent) <></>

    return (
      <ConfirmationDetail
        key={index}
        label={item?.label}
        lineContent={item?.lineContent}
        callbackFn={item?.callbackFn}
        isLastItem={txDetails.length - 1 === index}
      />
    )
  }

  const keyExtractor = () => Crypto.randomBytes(4).toString("base64")

  return (
    <SafeAreaView style={[isLightMode ? styles.safeArea_light : styles.safeArea_dark]}>
      <View style={styles.mainContainer}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <View style={styles.header}>
          <HeaderText colorScheme={colorScheme}>Tx Details</HeaderText>
        </View>
        <FlatList
          contentContainerStyle={styles.listContainer}
          data={txDetails}
          renderItem={renderItem}
          keyExtractor={keyExtractor}
        />
        <View>
          <FullWidthButton
            text={"Sign & Submit"}
            lightMode={isLightMode}
            disabled={false}
            onPressCallback={onSignAndSubmit}
          />
        </View>
      </View>
      {authenticatorVisible && (
        <Authenticator
          authRequestType="account-key"
          showAuthenticator={authenticatorVisible}
          onAuthenticatedCb={onAuthenticated}
          onHideAuthenticatorCb={onHideAuthenticator}
        />
      )}
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
    flex: 1,
  },
  safeArea_dark: {
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
    flex: 1,
  },
  mainContainer: {
    flex: 1,
    width: "90%",
    marginBottom: Sizing.x5,
  },
  header: {
    width: "100%",
    marginBottom: Sizing.x5,
  },
  navigation: {
    flexDirection: "row",
    marginVertical: Sizing.x15,
  },
  listContainer: {},
})
