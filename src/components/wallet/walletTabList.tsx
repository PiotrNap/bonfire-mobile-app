import * as React from "react"
import {
  View,
  StyleSheet,
  FlatList,
  RefreshControl,
  FlatListProps,
  ActivityIndicator,
} from "react-native"

import { appContext } from "contexts/contextApi"
import { Colors, Sizing, Typography } from "styles/index"

import { TransactionItem } from "./transactionItem"
import { AssetItem } from "./assetItem"
import Crypto from "crypto"
import { noop } from "lib/utils"

export interface TransactionListProps {
  listData: Map<string, any>
  // isSmallScreen: boolean
  isLoading: boolean
  onUpdateList: () => any
  onEndReached?: () => any
  type: "assets" | "transactions"
}

export const WalletTabList = ({
  listData,
  isLoading,
  onUpdateList,
  onEndReached,
  type,
}: TransactionListProps) => {
  const { colorScheme } = appContext()
  const [containerWidth, setContainerWidth] = React.useState<any>(0)
  const isLightMode = colorScheme === "light"
  const renderItem = React.useCallback(
    ({ item }: any) =>
      type === "transactions" ? (
        <TransactionItem item={item} />
      ) : (
        <AssetItem item={item} />
      ),
    []
  )

  const keyExtractor = () => Crypto.randomBytes(16).toString("base64")

  var flatListProps: FlatListProps<any> = {
    data: Array.from(listData.values()),
    renderItem: renderItem,
    keyExtractor: keyExtractor,
    contentOffset: { x: 0, y: -25 },
    refreshControl: (
      <RefreshControl
        tintColor={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
        refreshing={isLoading}
        onRefresh={onUpdateList}
      />
    ),
  }
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
    <View
      onLayout={onLayout}
      style={[styles.container, isLoading ? { opacity: 0.5 } : {}]}>
      <FlatList
        {...flatListProps}
        scrollEnabled={!isLoading}
        disableScrollViewPanResponder={isLoading}
        onEndReached={onEndReached}
        onEndReachedThreshold={0.1}
        getItemLayout={getItemLayout}
        windowSize={8}
      />
      {/*
      {isLoading && (
        <ActivityIndicator
          size={isSmallScreen ? "small" : "large"}
          color={Colors.primary.s800}
          style={styles.spinner}
        />
      )}
      */}
    </View>
  )
}

const styles = StyleSheet.create({
  container: { flex: 1, marginVertical: 10 },
  spinner: {
    position: "absolute",
    top: "50%",
    left: "50%",
  },
  innerHeader: {
    alignSelf: "center",
    marginTop: Sizing.x10,
    ...Typography.subHeader.x20,
  },
})
