import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Colors, Sizing } from "styles/index"

import { appContext } from "contexts/contextApi"
import { BlockFrostDetailedTx } from "lib/wallet/types"
import { RightArrowIcon, UpArrow } from "assets/icons"
import { DownArrow } from "assets/icons/downArrow"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { useNavigation } from "@react-navigation/native"

export const TransactionItem = React.memo(
  ({ item: transaction }: { item: BlockFrostDetailedTx }) => {
    const { colorScheme } = appContext()
    const isOutgoing = transaction.inputs.some(
      (input) => input.address === transaction.user_address
    )
    const navigation = useNavigation()
    const onTxItemPress = () =>
      navigation.navigate("Preview Transaction", {
        txInfo: transaction,
        isOutgoing,
        isTxHistoryPreview: true,
      })

    const assetCount = transaction.outputs[0].amount.length - 1 // Assuming lovelace is listed first
    const adaAmount = Number(transaction.outputs[0].amount[0].quantity) / 1000000 // Convert lovelace to ADA

    return (
      <View style={styles.container}>
        {isOutgoing ? (
          <UpArrow width={22} height={22} stroke={Colors.danger.s400} strokeWidth={2} />
        ) : (
          <DownArrow
            width={22}
            height={22}
            stroke={Colors.success.s400}
            strokeWidth={2}
          />
        )}
        <View style={styles.txInfo}>
          <Text>
            {new Date(Number(transaction.block_time) * 1000).toLocaleDateString() +
              " " +
              new Date(Number(transaction.block_time) * 1000).toLocaleTimeString()}
          </Text>
          <SubHeaderText
            customStyle={{ marginBottom: 0 }}
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            {`${adaAmount} ADA ${
              assetCount > 0
                ? `+ ${assetCount} ${assetCount > 1 ? "Assets" : "Asset"}`
                : ""
            }`}
          </SubHeaderText>
        </View>
        <Pressable hitSlop={Sizing.x10} onPress={onTxItemPress}>
          <RightArrowIcon
            width="20"
            height="20"
            strokeWidth={2}
            stroke={
              colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral
            }
            style={{ marginRight: "auto" }}
          />
        </Pressable>
      </View>
    )
  }
)

const styles = StyleSheet.create({
  container: {
    flexDirection: "row",
    alignItems: "center",
    marginVertical: Sizing.x10,
    height: Sizing.x55,
  },
  txInfo: {
    flex: 1,
    marginLeft: Sizing.x10,
  },
})
