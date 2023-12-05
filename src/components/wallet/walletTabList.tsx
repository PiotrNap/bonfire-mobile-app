import * as React from "react"
import {
  View,
  StyleSheet,
  FlatList,
  RefreshControl,
  ActivityIndicator,
} from "react-native"

import { appContext, walletContext } from "contexts/contextApi"
import { Colors, Sizing, Typography } from "styles/index"

import { TransactionItem } from "./transactionItem"
import { AssetItem } from "./assetItem"
import { AssetUnit } from "lib/wallet/types"
import Crypto from "crypto"

export interface TransactionListProps {
  listData: Map<string, any>
  isSmallScreen?: boolean
  isLoading?: boolean
  isPaginationLoading?: boolean
  isSendTransactionScreen?: boolean
  onUpdateList?: () => any
  onEndReached?: () => any
  onCheckboxPress?: (unit: string, count: number) => void
  type: "assets" | "transactions"
  selectedAssets?: Map<string, AssetUnit>
}

export const WalletTabList = React.memo(
  ({
    listData,
    isLoading = false,
    isSmallScreen = true,
    isPaginationLoading = false,
    isSendTransactionScreen = false,
    onCheckboxPress,
    selectedAssets,
    onUpdateList,
    onEndReached,
    type,
  }: TransactionListProps) => {
    const { colorScheme } = appContext()
    const [containerWidth, setContainerWidth] = React.useState<any>(0)
    const isLightMode = colorScheme === "light"

    const renderItem = React.useCallback(
      ({ item }: any) => {
        return type === "transactions" ? (
          <TransactionItem item={item} />
        ) : (
          <AssetItem
            item={item}
            isSendTransactionScreen={isSendTransactionScreen}
            onCheckboxPress={onCheckboxPress}
            isSelected={!!selectedAssets?.has(item?.policyId + item?.label + item?.name)}
          />
        )
      },
      [type, isSendTransactionScreen, selectedAssets]
    )

    const keyExtractor = () => Crypto.randomBytes(16).toString("base64")

    const refreshControl = (
      <RefreshControl
        tintColor={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
        refreshing={isLoading}
        onRefresh={onUpdateList}
      />
    )

    const getItemLayout = React.useCallback(
      (_, index: number) => ({
        length: containerWidth,
        offset: (Sizing.x55 + Sizing.x25) * index,
        index,
      }),
      [containerWidth]
    )
    const onLayout = (e) => setContainerWidth(e.nativeEvent.layout.width)

    return (
      <FlatList
        onLayout={onLayout}
        contentContainerStyle={isLoading ? { opacity: 0.5 } : {}}
        refreshControl={!isSendTransactionScreen ? refreshControl : undefined}
        data={Array.from(listData.values())}
        keyExtractor={keyExtractor}
        renderItem={renderItem}
        scrollEnabled={!isLoading}
        disableScrollViewPanResponder={isLoading}
        onEndReached={onEndReached}
        onEndReachedThreshold={0.3}
        getItemLayout={getItemLayout}
        ListFooterComponent={
          isPaginationLoading ? (
            <ActivityIndicator
              size={isSmallScreen ? "small" : "large"}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
              style={styles.spinner}
            />
          ) : null
        }
        windowSize={8}
      />
    )
  }
)

const styles = StyleSheet.create({
  spinner: {
    marginBottom: Sizing.x10,
  },
  innerHeader: {
    alignSelf: "center",
    marginTop: Sizing.x10,
    ...Typography.subHeader.x20,
  },
})
