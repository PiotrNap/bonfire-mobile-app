import * as React from "react";
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  LayoutChangeEvent,
  ActivityIndicator,
  NativeModules,
} from "react-native";

import { SafeAreaView } from "react-native-safe-area-context";
import { LinearGradient } from "expo-linear-gradient";
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index";
import { StackScreenProps } from "@react-navigation/stack";
import { OrganizerTabParamList } from "common/types/navigationTypes";
import { appContext } from "contexts/contextApi";
import { RefreshIcon, RightArrowIcon, SearchIcon } from "icons/index";
import { TransactionsList } from "components/wallet/transactionsList";
import { ProfileContext } from "contexts/profileContext";
import { Address } from "@emurgo/react-native-haskell-shelley";

// @TODO: Implement navigationTypes type
export interface WalletScreenProps
  extends StackScreenProps<OrganizerTabParamList, "Wallet"> {}

function wait(ms: number): Promise<void> {
  return new Promise((res) => setTimeout(res, ms));
}

export const WalletScreen = ({ navigation, route }: WalletScreenProps) => {
  const { colorScheme } = appContext();
  const { hasSyncedWallet, setHasSyncedWallet, walletBalance } =
    React.useContext(ProfileContext);
  const [layoutHeight, setLayoutHeight] = React.useState<any>(null);
  const [isSmallScreen, setIsSmallScreen] = React.useState<boolean>(false);
  const [isTxListLoading, setIsTxListLoading] = React.useState<boolean>(false);
  const [isWalletSyncing, setIsWalletSyncing] = React.useState<boolean>(false);

  const darkGradient: string[] = [Colors.primary.s800, Colors.primary.s600];
  const lightGradient: string[] = [Colors.primary.s200, Colors.primary.neutral];

  console.log(NativeModules.HaskellShelley);
  const getAddr = async () => {
    try {
      const addrHex =
        "616464725f746573743176707134356b72613075366d6c7233747261676a70323072307037756a703733356774367430676d77767466773273797a637a3378";
      const addrBytes = Buffer.from(addrHex, "hex");
      console.log("bytes ", addrBytes);
      const address = await Address.from_bytes(addrBytes);
      console.log("address is :", address);
    } catch (err) {
      console.error(err);
    }
  };

  React.useEffect(() => {
    getAddr();
  }, []);

  const onWalletButtonPress = async () => {
    if (!hasSyncedWallet) {
      setIsWalletSyncing(true);
      await wait(1200);
      setHasSyncedWallet(true);
      setIsWalletSyncing(false);
    } else {
      navigation.navigate("Add Funds", { fromScreen: "Wallet" });
    }
  };

  const onLayout = (e: LayoutChangeEvent) => {
    setLayoutHeight(e.nativeEvent.layout.height);

    if (layoutHeight && layoutHeight < 300) {
      setIsSmallScreen(true);
    }
  };

  const onRefreshPress = async () => {
    setIsTxListLoading(true);
    await wait(2000);
    setIsTxListLoading(false);
  };

  return (
    <SafeAreaView
      style={[
        colorScheme == "light" ? styles.safeArea_light : styles.safeaArea_dark,
      ]}>
      <View style={styles.container}>
        {/* <View style={styles.searchToolContainer}>
          </View>*/}

        <LinearGradient
          colors={colorScheme === "light" ? darkGradient : lightGradient}
          start={{ x: 0, y: 1 }}
          end={{ x: 1, y: 0 }}
          style={styles.walletContainer}>
          <ActivityIndicator
            animating={isWalletSyncing}
            color={
              colorScheme === "light"
                ? Colors.primary.neutral
                : Colors.primary.s800
            }
            style={styles.walletLoadingSpinner}
          />
          <Text
            style={[
              colorScheme == "light"
                ? styles.walletHeader_ligth
                : styles.walletHeader_dark,
              { marginRight: "auto" },
            ]}>
            Current balance
          </Text>
          <Text
            style={[
              colorScheme == "light"
                ? styles.walletBalance_ligth
                : styles.walletBalance_dark,
            ]}>
            {!hasSyncedWallet ? "--" : walletBalance} ₳
          </Text>
          <Pressable
            onPress={onWalletButtonPress}
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
              {!hasSyncedWallet ? "Link Wallet" : "Add funds"}
            </Text>
          </Pressable>
        </LinearGradient>
        <View onLayout={onLayout} style={styles.transactionsContainer}>
          <View style={styles.transactionsHeaderContainer}>
            <View style={styles.transactionsHeader}>
              <Text
                style={
                  colorScheme === "light"
                    ? styles.transactionsHeaderText_light
                    : styles.transactionsHeaderText_dark
                }>
                Transactions
              </Text>
              <Text
                style={
                  colorScheme === "light"
                    ? styles.transactionsSubheaderText_light
                    : styles.transactionsSubheaderText_dark
                }>
                Last 30 days
              </Text>
            </View>
            <View style={styles.iconWrapper}>
              {isSmallScreen && (
                <Pressable onPress={onRefreshPress} hitSlop={5}>
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
                  colorScheme === "light"
                    ? Colors.primary.s800
                    : Colors.primary.neutral
                }
                strokeWidth={1.8}
                style={{ marginLeft: "auto" }}
              />
            </View>
          </View>
          <TransactionsList
            isLoading={isTxListLoading}
            isSmallScreen={isSmallScreen}
          />
        </View>
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeaArea_dark: {
    flex: 1,
    backgroundColor: Colors.primary.s600,
    alignItems: "center",
  },
  container: {
    flex: 1,
    width: "90%",
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
    fontSize: Sizing.x65,
    color: Colors.primary.neutral,
  },
  walletBalance_dark: {
    fontFamily: "Roboto-Medium",
    fontSize: Sizing.x65,
    color: Colors.primary.s800,
  },
  walletButton_light: {
    paddingVertical: Sizing.x1,
    paddingHorizontal: Sizing.x8,
    borderWidth: Sizing.x3,
    borderColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    marginBottom: Sizing.x20,
    backgroundColor: "transparent",
  },
  walletButton_dark: {
    paddingVertical: Sizing.x2,
    paddingHorizontal: Sizing.x7,
    borderWidth: 4,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.base,
    marginBottom: Sizing.x20,
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
  transactionsContainer: {
    flex: 1,
    marginTop: Sizing.x40,
  },
  transactionsHeaderContainer: {
    flexDirection: "row",
    alignItems: "center",
  },
  transactionsHeader: {},
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
});
