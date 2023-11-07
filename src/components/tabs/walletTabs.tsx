import React, { useState } from "react"
import { WalletTabList } from "components/wallet/walletTabList"
import { View, Text, TouchableOpacity, StyleSheet } from "react-native"
import { Sizing, Typography } from "styles/index"
import { appContext } from "contexts/contextApi"
import { schemeBasedFontColor } from "../../styles/typography"
import { noop } from "lib/utils"

export const WalletTabs = ({
  assets,
  transactions,
  onAssetsListUpdate,
  onTxListUpdate,
  onTxListEndReached,
  isLoading,
}: any) => {
  const { colorScheme, bottomNavigationHeight } = appContext()
  const [activeTab, setActiveTab] = useState("assets")

  return (
    <View style={[styles.container, { marginBottom: bottomNavigationHeight / 2 }]}>
      <View style={styles.tabBar}>
        <TouchableOpacity
          style={[
            styles.tab,
            activeTab === "assets" && styles.activeTab,
            { borderColor: schemeBasedFontColor(colorScheme) },
          ]}
          onPress={() => setActiveTab("assets")}>
          <Text style={[styles.tabText, { color: schemeBasedFontColor(colorScheme) }]}>
            Assets
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          style={[
            styles.tab,
            activeTab === "history" && styles.activeTab,
            { borderColor: schemeBasedFontColor(colorScheme) },
          ]}
          onPress={() => setActiveTab("history")}>
          <Text style={[styles.tabText, { color: schemeBasedFontColor(colorScheme) }]}>
            Transactions
          </Text>
        </TouchableOpacity>
      </View>
      {activeTab === "assets" && (
        <View style={styles.tabContent}>
          <WalletTabList
            listData={assets}
            onUpdateList={onAssetsListUpdate}
            onEndReached={noop}
            isLoading={isLoading}
            type="assets"
          />
        </View>
      )}
      {activeTab === "history" && (
        <View style={styles.tabContent}>
          <WalletTabList
            listData={transactions}
            onUpdateList={onTxListUpdate}
            onEndReached={onTxListEndReached}
            isLoading={isLoading}
            type="transactions"
          />
        </View>
      )}
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    flex: 1,
  },
  tabBar: {
    flexDirection: "row",
    borderBottomWidth: 1,
    borderBottomColor: "#ccc",
  },
  tab: {
    flex: 1,
    padding: Sizing.x15,
    alignItems: "center",
    justifyContent: "center",
  },
  activeTab: {
    borderBottomWidth: 2,
    borderBottomColor: "#000",
  },
  tabContent: {
    height: "100%",
    widht: "100%",
    marginHorizontal: Sizing.x10,
  },
  tabText: {
    ...Typography.header.x20,
  },
})