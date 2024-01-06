import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Colors, Sizing } from "styles/index"

import { appContext } from "contexts/contextApi"
import { BlockFrostDetailedTx, BlockFrostUtxoInfo } from "lib/wallet/types"
import { RightArrowIcon, UpArrow } from "assets/icons"
import { DownArrow } from "assets/icons/downArrow"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { useNavigation } from "@react-navigation/native"
import { MAINNET_ESCROW_CONTRACT_ADDRESS, TESTNET_ESCROW_CONTRACT_ADDRESS } from "@env"

export const TransactionItem = React.memo(
  ({ item: transaction }: { item: BlockFrostDetailedTx }) => {
    const { colorScheme, networkId } = appContext()
    const [assetCount, setAssetCount] = React.useState<number>(0)
    const [adaAmount, setAdaAmount] = React.useState<number>(0)

    const inputFromEscrowContract = transaction.inputs.find(
      (input) =>
        input.address ===
        (networkId === "Mainnet"
          ? MAINNET_ESCROW_CONTRACT_ADDRESS
          : TESTNET_ESCROW_CONTRACT_ADDRESS)
    )
    // eg. during event cancellation
    const isOutgoingFromEscrowContract = false
    // inputFromEscrowContract &&
    // transaction.outputs.find(
    //   (out) =>
    //     out.amount.find((amt) => amt.unit === "lovelace")?.quantity ===
    //     inputFromEscrowContract.amount.find((amt) => amt.unit === "lovelace")?.quantity
    // )?.address !== transaction.user_address

    // transaction is outgoing IF more assets were spent than received
    // @TODO after release; implement the above logic
    const isOutgoing = transaction.inputs.some(
      (input) => input.address === transaction.user_address
    )
    const navigation = useNavigation()
    const onTxItemPress = () =>
      navigation.navigate("Preview Transaction", {
        txInfo: transaction,
        isOutgoing,
        isIncomingFromEscrowContract: inputFromEscrowContract,
        isOutgoingFromEscrowContract,
        isTxHistoryPreview: true,
      })

    React.useLayoutEffect(() => {
      let assetCount = inputFromEscrowContract
        ? transaction.outputs.filter(
            (out) => out.address === transaction.user_address && out.amount.length > 1
          ).length - 1
        : transaction.outputs[0].amount.length - 1 // Assuming lovelace is listed first
      let adaAmount = isOutgoingFromEscrowContract
        ? 0
        : Number(
            transaction.outputs[inputFromEscrowContract ? 1 : 0].amount[0].quantity
          ) / 1000000 // Convert lovelace to ADA

      setAssetCount(assetCount)
      setAdaAmount(adaAmount)
    }, [transaction])

    return (
      <View style={styles.container}>
        {(!inputFromEscrowContract && isOutgoing) || isOutgoingFromEscrowContract ? (
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
          <Text
            style={{
              color:
                colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral,
            }}>
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
        <Pressable hitSlop={Sizing.x15} onPress={onTxItemPress}>
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
