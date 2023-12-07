import React from "react"

import { AdaIcon, DuplicateIcon, LeftArrowIcon } from "assets/icons"
import { SectionDetail } from "common/interfaces/bookingInterface"
import { ConfirmationDetail } from "components/booking"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, walletContext } from "contexts/contextApi"
import { FlatList, Pressable, SafeAreaView, StyleSheet, View } from "react-native"
import { Colors, Sizing } from "styles/index"
import {
  cutStringInside,
  fromAssetUnit,
  hexToUtf8,
  lovelaceToAda,
} from "lib/wallet/utils"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { showErrorToast } from "lib/helpers"
import { Wallet } from "lib/wallet"
import { Authenticator } from "components/modals/Authenticator"
import { getRandomKey } from "lib/utils"
import Clipboard from "@react-native-clipboard/clipboard"
import Crypto from "crypto"

export function PreviewTransactionScreen({ navigation, route }: any) {
  const { colorScheme, setQrCodeValue } = appContext()
  const { sendTxInfo, baseAddress, walletUtxos, setSendTxInfo } = walletContext()
  const [authenticatorVisible, setAuthenticatorVisible] = React.useState<boolean>(false)
  const { params } = route
  const isLightMode = colorScheme === "light"
  const isTxHistoryPreview = params?.isTxHistoryPreview
  const isIncomingFromSmartContract = params?.isIncomingFromSmartContract
  const isOutgoing = params?.isOutgoing
  const txInfo = isTxHistoryPreview ? params?.txInfo : sendTxInfo
  const isSelfFundedTx = React.useMemo(
    () =>
      params?.txInfo.inputs.every(
        (input) => input.address === params?.txInfo.user_address
      ) &&
      params?.txInfo.outputs.every(
        (output) => output.address === params?.txInfo.user_address
      ),
    [params?.txInfo]
  )
  const onBackNavigationPress = () => navigation.goBack()

  const iconStyles = {
    stroke: isLightMode ? Colors.primary.s800 : Colors.primary.s200,
    strokeWidth: 1.8,
    width: Sizing.x25,
    height: Sizing.x25,
    marginRight: Sizing.x5,
    zIndex: -10,
  }
  const onSignAndSubmit = () => {
    if (!txInfo) return showErrorToast("Missing send transaction info")
    setAuthenticatorVisible(true)
  }
  const onAuthenticated = async (accountKey?: string | void) => {
    if (!txInfo) {
      showErrorToast("Missing send transaction info")
      accountKey = ""
      return
    }
    if (!accountKey) return showErrorToast("Something went wrong. Missing signing key.")
    try {
      await Wallet.sendRegularTransaction(txInfo, baseAddress, walletUtxos, accountKey)

      setSendTxInfo({})
      setAuthenticatorVisible(false)
      setQrCodeValue("")
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
  const copyTxHash = () => {
    Clipboard.setString(txInfo.hash)
  }
  const getOutputAssets = React.useMemo(() => {
    return txInfo?.outputs
      ?.filter((out, idx) => {
        if (isIncomingFromSmartContract) {
          return out.output_index === 0
        } else if (isOutgoing) {
          return out.address !== txInfo.user_address
        } else return out.address === txInfo.user_address
      })
      .map((out) => {
        return out.amount.map((amount) =>
          amount.unit === "lovelace"
            ? ""
            : `${hexToUtf8(fromAssetUnit(amount.unit).name)} - (${Number(
                amount.quantity
              ).toFixed(2)})`
        )
      })
      .flat()
      .filter((asset) => asset) // filter out empty string
      .map((asset) => ({ content: asset })) // this is required to display in UI
  }, [txInfo])

  const txHistoryDetails: any[] = [
    {
      label: "Hash",
      lineContent: {
        content: cutStringInside(txInfo.hash),
      },
      callbackFn: {
        icon: <DuplicateIcon {...iconStyles} />,
        onPress: copyTxHash,
      },
    },
    {
      label: "To",
      lineContent:
        isIncomingFromSmartContract || !isOutgoing || isSelfFundedTx
          ? { content: cutStringInside(txInfo?.user_address) }
          : txInfo?.outputs.map((out) =>
              out.address === txInfo.user_address
                ? null
                : { content: cutStringInside(out.address) }
            ),
    },
    {
      label: "ADA",
      lineContent: {
        content: isSelfFundedTx
          ? lovelaceToAda(
              txInfo.outputs[0].amount.find((amt) => amt.unit === "lovelace").quantity
            )
          : lovelaceToAda(
              BigInt(
                txInfo?.outputs
                  ?.filter((out) => {
                    if (isIncomingFromSmartContract) {
                      return out.output_index === 0
                    } else if (isOutgoing) {
                      return out.address !== txInfo.user_address
                    } else return out.address === txInfo.user_address
                  })
                  .map((out) => {
                    return out.amount.find((amount) => amount.unit === "lovelace")
                      .quantity
                  })
                  .reduce((prev, acc) => Number(acc) + prev, 0) || 0
              )
            ),
        icon: <AdaIcon {...iconStyles} />,
      },
    },
    getOutputAssets?.length && {
      label: "Assets",
      lineContent: getOutputAssets,
    },
  ].filter((txDetail) => txDetail)

  const txSendDetails: any[] = [
    {
      label: "To",
      lineContent: {
        content: cutStringInside(txInfo?.receiverAddress),
      },
    },
    {
      label: "ADA",
      lineContent: {
        content: lovelaceToAda(BigInt(txInfo?.lovelace || 0)).toFixed(2),
        icon: <AdaIcon {...iconStyles} />,
      },
    },
    txInfo?.assets?.size && {
      label: "Assets",
      lineContent: Array.from(txInfo?.assets).map((asset: any) => {
        return {
          content: `${hexToUtf8(asset[1].name)} - (${Number(asset[1].count).toFixed(2)})`,
        }
      }),
    },
  ].filter((txDetail) => txDetail)

  const renderItem = ({ item, index }: { item: SectionDetail; index: number }) => {
    if (!item.lineContent) <></>

    const detailsList = isTxHistoryPreview ? txHistoryDetails : txSendDetails

    return (
      <ConfirmationDetail
        key={getRandomKey(2)}
        label={item?.label}
        //@ts-ignore because of the `lineContent` that can be an array
        lineContent={item?.lineContent}
        callbackFn={item?.callbackFn}
        isLastItem={detailsList.length - 1 === index}
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
          data={isTxHistoryPreview ? txHistoryDetails : txSendDetails}
          renderItem={renderItem}
          keyExtractor={keyExtractor}
        />
        {!isTxHistoryPreview && (
          <View>
            <FullWidthButton
              text={"Sign & Submit"}
              lightMode={isLightMode}
              disabled={false}
              onPressCallback={onSignAndSubmit}
            />
          </View>
        )}
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
