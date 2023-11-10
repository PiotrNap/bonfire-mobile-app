import * as React from "react"
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  ActivityIndicator,
  Animated,
} from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import LinearGradient from "react-native-linear-gradient"
import { useFocusEffect } from "@react-navigation/native"
import { StackScreenProps } from "@react-navigation/stack"
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index"
import { WalletStackParamList } from "common/types/navigationTypes"
import { appContext, walletContext } from "contexts/contextApi"
import { DownIcon, UpIcon } from "icons/index"
import { ProfileContext } from "contexts/profileContext"
import { getFromEncryptedStorage } from "lib/encryptedStorage"
import {
  assetsToUnitsMap,
  filterLovelaceOnlyInputs,
  lovelaceToAda,
  lovelaceValueOfInputs,
  txInputsToAssets,
} from "lib/wallet/utils"
import { TX_GET_SIZE, Wallet } from "lib/wallet"
import { WalletTabs } from "components/tabs/walletTabs"
import { Toast } from "react-native-toast-message/lib/src/Toast"
import { Crypto } from "@hyperionbt/helios"
import { BlockFrostDetailedTx } from "lib/wallet/types"
import AsyncStorage from "@react-native-async-storage/async-storage"
import { showErrorToast } from "lib/helpers"

export interface WalletScreenProps
  extends StackScreenProps<WalletStackParamList, "Wallet Main"> {}

export const WalletScreen = ({ navigation, route }: WalletScreenProps) => {
  const { colorScheme } = appContext()
  const {
    txHistory,
    setTxHistory,
    baseAddress,
    setBaseAddress,
    setLovelaceBalance,
    setWalletUtxos,
    walletAssets,
    setWalletAssets,
    lovelaceBalance,
  } = walletContext()
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [isPaginationLoading, setIsPaginationLoading] = React.useState<boolean>(false)
  const [txListPage, setTxListPage] = React.useState<number>(1)
  const [txHistoryEndReached, setTxHistoryEndReached] = React.useState<boolean>(false)
  const [lockedLovelaceBalance, setLockedLovelaceBalance] = React.useState<bigint>(0n)

  const [aU, setaU] = React.useState<any>(1)
  const [tU, settU] = React.useState<any>(1)

  const updateWalletBalance = React.useCallback(
    async (addr?: string) => {
      try {
        console.log("tokens update :", aU)
        setaU((p) => p + 1)

        setIsLoading(true)
        addr = addr ?? baseAddress

        const isGoodAddr = Crypto.verifyBech32(addr)
        if (!isGoodAddr)
          Toast.show({
            type: "error",
            text1: "Unexpected address",
            text2: "Cannot fetch address assets.",
          })

        const { data, error } = await Wallet.getUtxosAtAddress(addr)

        if (!data || error)
          Toast.show({
            type: "error",
            text1: error?.error || "Unable to get assets",
            text2: "Maybe try again?",
          })
        if (!data.length) return setIsLoading(false)

        const availableLovelace = lovelaceValueOfInputs(filterLovelaceOnlyInputs(data))
        const lockedLovelace = lovelaceValueOfInputs(data) - availableLovelace
        setLovelaceBalance(availableLovelace)
        setLockedLovelaceBalance(lockedLovelace)

        // extracts assets from TxInputs into a Map
        const assets = assetsToUnitsMap(txInputsToAssets(data))
        setWalletAssets(assets)

        setWalletUtxos(data)
        setIsLoading(false)
      } catch (e) {
        showErrorToast(e)
      }
    },
    [baseAddress]
  )
  const updateWalletTxHistory = React.useCallback(
    async (addr?: string, refresh = true) => {
      try {
        if (!refresh) {
          if (txHistoryEndReached) return // no more pagination
          setIsPaginationLoading(true)
        }

        console.log("txs update :", tU)
        settU((p) => p + 1)

        addr = addr ?? baseAddress
        const isGoodAddr = Crypto.verifyBech32(addr)
        if (!isGoodAddr)
          Toast.show({
            type: "error",
            text1: "Unexpected address",
            text2: "Cannot fetch address transactions.",
          })

        const { data: transactions, error } = await Wallet.getTransactionsAtAddress(
          addr,
          refresh ? 1 : txListPage
        )
        if (!transactions || error)
          Toast.show({
            type: "error",
            text1: error?.error || "Unable to get transactions",
            text2: "Maybe try again?",
          })
        if (!transactions.length) {
          setTxHistoryEndReached(true) // no tx's left at this address
          return setIsPaginationLoading(false)
        }
        if (transactions.length < TX_GET_SIZE) {
          setTxHistoryEndReached(true) // we've reached the end
        }

        let fullInfoTxs: BlockFrostDetailedTx[] = []

        for (let transaction of transactions) {
          const { data: tx, error } = await Wallet.getTxUtxos(transaction?.tx_hash)

          if (error)
            Toast.show({
              type: "error",
              text1: error?.error || "Unable to get transaction info",
              text2: "Tx-hash: " + transaction?.tx_hash,
            })
          tx.block_time = transaction.block_time
          tx.user_address = addr
          fullInfoTxs.push(tx)
        }
        fullInfoTxs = fullInfoTxs.reverse() // because Blockfrost can't sort them
        if (refresh) {
          setTxHistory(fullInfoTxs)
          setTxListPage(1)
        } else {
          setTxHistory([...txHistory, ...fullInfoTxs])
          setTxListPage((prev) => prev + 1)
        }
        setIsPaginationLoading(false)
      } catch (e) {
        showErrorToast(e)
      }
    },
    [baseAddress, txHistory, txListPage]
  )
  const getNextPageWalletTxHistory = () => updateWalletTxHistory(baseAddress, false)

  /** Update wallet Utxos and ADA balance **/
  useFocusEffect(
    React.useCallback(() => {
      ;(async () => {
        let addr: any = baseAddress
        if (!addr) {
          addr = await AsyncStorage.getItem("account-#0-baseAddress")
          setBaseAddress(addr || "")
        }
        updateWalletBalance(addr)
        updateWalletTxHistory(addr)
      })()
    }, [])
  )

  const darkGradient: string[] = [Colors.primary.s800, Colors.primary.s600]
  const lightGradient: string[] = [Colors.primary.s200, Colors.primary.neutral]
  const onSendPress = () => {
    navigation.navigate("Send Transaction")
  }
  const onReceivePress = () => {
    navigation.navigate("Receive Transaction")
  }
  const adaDisplayValue = lovelaceToAda(lovelaceBalance).toFixed(2)
  const lockedAdaDisplayValue = lovelaceToAda(lockedLovelaceBalance).toFixed(2)

  return (
    <SafeAreaView
      style={[colorScheme == "light" ? styles.safeArea_light : styles.safeaArea_dark]}>
      <View style={[styles.container]}>
        <LinearGradient
          colors={colorScheme === "light" ? darkGradient : lightGradient}
          start={{ x: 0, y: 1 }}
          end={{ x: 1, y: 0 }}
          style={styles.walletContainer}>
          <ActivityIndicator
            animating={isLoading}
            color={colorScheme === "light" ? Colors.primary.neutral : Colors.primary.s800}
            style={styles.walletLoadingSpinner}
          />
          <View style={styles.balanceWrapper}>
            <Text
              style={[
                colorScheme == "light"
                  ? styles.walletBalance_ligth
                  : styles.walletBalance_dark,
              ]}>
              {adaDisplayValue} ₳
            </Text>
            {Number(lockedAdaDisplayValue) > 0 ? (
              <Text
                style={[
                  colorScheme == "light"
                    ? styles.lockedBalance_ligth
                    : styles.lockedBalance_dark,
                ]}>
                (+ {lockedAdaDisplayValue} ₳ in assets )
              </Text>
            ) : (
              <></>
            )}
          </View>
          <View style={styles.walletButtonsContainer}>
            <Pressable
              disabled={isLoading}
              onPress={onSendPress}
              pressRetentionOffset={15}
              hitSlop={15}
              style={Buttons.applyOpacity(
                colorScheme == "light"
                  ? styles.walletButton_light
                  : styles.walletButton_dark
              )}>
              <Text
                style={[
                  colorScheme == "light"
                    ? styles.walletButtonText_light
                    : styles.walletButtonText_dark,
                ]}>
                Send
              </Text>
              <UpIcon
                width={22}
                height={22}
                stroke={
                  colorScheme === "dark" ? Colors.primary.s800 : Colors.primary.neutral
                }
                strokeWidth={2}
              />
            </Pressable>
            <Pressable
              disabled={isLoading}
              onPress={onReceivePress}
              pressRetentionOffset={15}
              hitSlop={15}
              style={Buttons.applyOpacity(
                colorScheme == "light"
                  ? styles.walletButton_light
                  : styles.walletButton_dark
              )}>
              <Text
                style={[
                  colorScheme == "light"
                    ? styles.walletButtonText_light
                    : styles.walletButtonText_dark,
                ]}>
                Receive
              </Text>
              <DownIcon
                width={22}
                height={22}
                stroke={
                  colorScheme === "dark" ? Colors.primary.s800 : Colors.primary.neutral
                }
                strokeWidth={2}
              />
            </Pressable>
          </View>
        </LinearGradient>
        {/*
          <View style={styles.transactionsHeaderContainer}>
            <View style={styles.iconWrapper}>
              {isSmallScreen && (
                <Pressable onPress={()=>updateWalletBalance()} hitSlop={5}>
                  <RefreshIcon
                    width={22}
                    height={22}
                    stroke={
                      colorScheme === "light"
                        ? !isTxListLoading
                          ? Colors.primary.s600
                          : Colors.primary.s800
                        : Colors.primary.neutral
                    }
                    strokeWidth={!isTxListLoading ? 1.5 : 1.6}
                  />
                </Pressable>
              )}
              <SearchIcon
                width={24}
                height={24}
                stroke={
                  colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral
                }
                strokeWidth={1.8}
                style={{ marginLeft: "auto" }}
              />
            </View>
          </View>
          */}
        <WalletTabs
          assets={walletAssets}
          transactions={txHistory}
          onAssetsListUpdate={updateWalletBalance}
          onTxListUpdate={updateWalletTxHistory}
          onTxListEndReached={getNextPageWalletTxHistory}
          isLoading={isLoading}
          isPaginationLoading={isPaginationLoading}
        />
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeaArea_dark: {
    flex: 1,
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
  },
  container: {
    flex: 1,
    width: "90%",
    marginBottom: Sizing.x20,
  },
  animatedView: {
    marginTop: Sizing.x10,
  },
  checkboxWrapper: {
    flexDirection: "row",
    alignItems: "center",
    marginVertical: Sizing.x5,
  },
  searchToolContainer: {
    alignItems: "flex-end",
    marginTop: Sizing.x10,
  },
  walletContainer: {
    height: Sizing.x130,
    alignItems: "center",
    justifyContent: "space-between",
    marginTop: Sizing.x20,
    padding: Sizing.x14,
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.lifted,
  },
  walletHeader_ligth: {
    ...Typography.header.x30,
    color: Colors.primary.s180,
  },
  walletHeader_dark: {
    ...Typography.header.x30,
    color: Colors.primary.s600,
  },
  walletBalance_ligth: {
    fontFamily: "Roboto-Medium",
    fontSize: Sizing.x50,
    color: Colors.primary.neutral,
  },
  walletBalance_dark: {
    fontFamily: "Roboto-Medium",
    fontSize: Sizing.x50,
    color: Colors.primary.s800,
  },
  lockedBalance_ligth: {
    fontFamily: "Roboto-Regular",
    fontSize: Sizing.x15,
    color: Colors.primary.neutral,
  },
  lockedBalance_dark: {
    fontFamily: "Roboto-Regular",
    fontSize: Sizing.x15,
    color: Colors.primary.s800,
  },
  balanceWrapper: {
    alignItems: "center",
  },
  walletButtonsContainer: {
    flexDirection: "row",
    width: "100%",
  },
  walletButton_light: {
    flex: 1,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    paddingVertical: Sizing.x5,
    paddingHorizontal: Sizing.x12,
    borderWidth: Sizing.x3,
    borderColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    marginBottom: Sizing.x20,
    marginHorizontal: Sizing.x5,
    backgroundColor: "transparent",
  },
  walletButton_dark: {
    flex: 1,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    paddingVertical: Sizing.x2,
    paddingHorizontal: Sizing.x7,
    borderWidth: Sizing.x3,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.base,
    marginBottom: Sizing.x20,
    marginHorizontal: Sizing.x5,
    backgroundColor: "transparent",
  },
  walletButtonText_light: {
    ...Typography.header.x25,
    textAlign: "center",
    color: Colors.primary.neutral,
  },
  walletButtonText_dark: {
    ...Typography.header.x30,
    textAlign: "center",
    color: Colors.primary.s800,
  },
  tabsContainer: {
    flex: 1,
  },
  transactionsHeaderContainer: {
    flexDirection: "row",
    alignItems: "center",
  },
  transactionsHeaderText_light: {
    ...Typography.header.x35,
    marginBottom: Sizing.x5,
    color: Colors.primary.s800,
  },
  transactionsHeaderText_dark: {
    ...Typography.header.x35,
    marginBottom: Sizing.x5,
    color: Colors.primary.neutral,
  },
  transactionsSubheaderText_light: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s600,
  },
  transactionsSubheaderText_dark: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s180,
  },
  iconWrapper: {
    width: "20%",
    flexDirection: "row",
    marginLeft: "auto",
    alignItems: "center",
  },
  walletLoadingSpinner: {
    position: "absolute",
    top: Sizing.x12,
    right: Sizing.x12,
  },
})
