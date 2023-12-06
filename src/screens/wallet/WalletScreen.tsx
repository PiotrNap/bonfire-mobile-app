import * as React from "react"
import { View, Text, StyleSheet, Pressable, ActivityIndicator } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import LinearGradient from "react-native-linear-gradient"
import { StackScreenProps } from "@react-navigation/stack"
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index"
import { WalletStackParamList } from "common/types/navigationTypes"
import { appContext, walletContext } from "contexts/contextApi"
import { DownIcon, UpIcon } from "icons/index"
import { COLLATERAL_LOVELACE, lovelaceToAda } from "lib/wallet/utils"
import { WalletTabs } from "components/tabs/walletTabs"
import { useWallet } from "lib/hooks/useWallet"
import { ProfileContext } from "contexts/profileContext"

export interface WalletScreenProps
  extends StackScreenProps<WalletStackParamList, "Wallet Main"> {}

export const WalletScreen = ({ navigation, route }: WalletScreenProps) => {
  const { collateralUtxoId } = React.useContext(ProfileContext)
  const { colorScheme } = appContext()
  const { txHistory, baseAddress, walletAssets, lovelaceBalance } = walletContext()
  const {
    updateWalletTxHistory,
    lockedLovelaceBalance,
    isLoading,
    isPaginationLoading,
    updateWalletBalance,
  } = useWallet(false)
  const getNextPageWalletTxHistory = () => updateWalletTxHistory(baseAddress, false)

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
            {/*
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
            )}*/}
            {/*
            {collateralUtxoId && (
              <Text
                style={[
                  colorScheme == "light"
                    ? styles.lockedBalance_ligth
                    : styles.lockedBalance_dark,
                ]}>
                (+ {lovelaceToAda(COLLATERAL_LOVELACE)} ₳ in collateral )
              </Text>
            )}
            */}
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
