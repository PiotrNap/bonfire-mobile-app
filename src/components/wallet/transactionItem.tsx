import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { Buttons, Colors, Sizing } from "styles/index"

import { appContext } from "contexts/contextApi"
import { BlockFrostDetailedTx, BlockFrostUtxoInfo } from "lib/wallet/types"
import { RightArrowIcon, UpArrow } from "assets/icons"
import { DownArrow } from "assets/icons/downArrow"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { useNavigation } from "@react-navigation/native"
import {
  MAINNET_ESCROW_CONTRACT_ADDRESS,
  TESTNET_ESCROW_CONTRACT_ADDRESS,
  TREASURY_PKH,
} from "@env"
import { COLLATERAL_LOVELACE, lovelaceToAda, reduceTxAmount } from "lib/wallet/utils"
import { Address, bytesToHex, Datum, hexToBytes, PubKeyHash } from "@helios-lang/compat"
import { DatumHash } from "@helios-lang/ledger-babbage"
import { decodeUplcData } from "@helios-lang/uplc"
import { ArrowUpDown, CircleArrowRight } from "lucide-react-native"
import { UplcData } from "@hyperionbt/helios"

export const TransactionItem = React.memo(
  ({ item: transaction }: { item: BlockFrostDetailedTx }) => {
    const { colorScheme, networkId } = appContext()
    const [type, setType] = React.useState<"in" | "out" | null>(null)
    const [lovelace, setLovelace] = React.useState<number | undefined>(0)
    const [assetCount, setAssetCount] = React.useState<number>(0)
    const [assets, setAssets] = React.useState<any[]>([])

    const escrowUtxoInput = transaction.inputs.find(
      (input) =>
        input.address ===
        (networkId === "Mainnet"
          ? MAINNET_ESCROW_CONTRACT_ADDRESS
          : TESTNET_ESCROW_CONTRACT_ADDRESS)
    )

    const escrowUtxoOutput = transaction.outputs.find(
      (output) =>
        output.address ===
        (networkId === "Mainnet"
          ? MAINNET_ESCROW_CONTRACT_ADDRESS
          : TESTNET_ESCROW_CONTRACT_ADDRESS)
    )

    const userAddr = transaction.user_address
    const treasuryAddr = Address.fromHash(
      networkId === "Mainnet",
      new PubKeyHash(TREASURY_PKH)
    ).toBech32()
    const noEscrowContractInteraction = !escrowUtxoInput && !escrowUtxoOutput
    const escrowedUtxo = transaction.outputs.find((out) => {
      let escrowedAssets = escrowUtxoInput?.amount.map((amt) => amt.unit)

      return (
        out.amount.length === escrowedAssets?.length &&
        out.amount.every((amt) => escrowedAssets?.includes(amt.unit))
      )
    })

    const userInputs = transaction.inputs.filter((input) => input.address === userAddr)
    const escrowUtxoOutputReceiver = escrowedUtxo?.address
    const escrowedUtxoBeneficiary = escrowUtxoInput?.inline_datum
      ? decodeUplcData(escrowUtxoInput?.inline_datum)?.items[0].bytes
      : null
    const escrowedUtxoBeneficiaryAddress =
      escrowedUtxoBeneficiary &&
      Address.fromHash(
        networkId === "Mainnet",
        new PubKeyHash(escrowedUtxoBeneficiary)
      ).toBech32()
    const cancellationFee =
      userInputs &&
      escrowUtxoOutputReceiver === userAddr &&
      transaction.outputs.find(
        (out) => out.address != userAddr && out.address != treasuryAddr
      ) &&
      transaction.outputs.find((out) => out.address === escrowedUtxoBeneficiaryAddress)

    const serviceFee = transaction.outputs.find((out) => out.address === treasuryAddr)

    const collateralSplitTx =
      noEscrowContractInteraction &&
      transaction.inputs.every(
        (out) => BigInt(out.amount[0].quantity) !== COLLATERAL_LOVELACE
      ) &&
      transaction.outputs.some(
        (out) => BigInt(out.amount[0].quantity) === COLLATERAL_LOVELACE
      )

    const navigation = useNavigation()
    const onTxItemPress = () =>
      navigation.navigate("Preview Transaction", {
        txInfo: transaction,
        isOutgoing: type === "out",
        lovelace: lovelace,
        assets,
        isIncomingFromEscrowContract:
          escrowUtxoInput && escrowUtxoOutputReceiver === userAddr,
        isOutgoingFromEscrowContract: !!escrowUtxoInput,
        isTxHistoryPreview: true,
      })

    React.useLayoutEffect(() => {
      if (escrowUtxoInput && escrowUtxoOutputReceiver === userAddr) {
        setType("in")
        if (cancellationFee) {
          setLovelace(
            Number(escrowedUtxo?.amount[0].quantity || 0) -
              Number(cancellationFee.amount[0].quantity)
          )
        } else if (serviceFee) {
          setLovelace(
            Number(escrowUtxoInput?.amount[0].quantity || 0) -
              Number(serviceFee.amount[0].quantity)
          )
        } else setLovelace(Number(escrowUtxoInput?.amount[0].quantity || 0))

        setAssetCount(Number(escrowUtxoInput?.amount.length || 0) - 1)
        setAssets((p) => {
          let newAssets = [...escrowUtxoInput.amount.slice(1)]
          return newAssets
        })
      } else if (
        (escrowUtxoInput &&
          escrowUtxoOutputReceiver &&
          escrowUtxoOutputReceiver !== userAddr) ||
        collateralSplitTx
      ) {
      } else if (escrowUtxoOutput) {
        setType("out")
        setLovelace(Number(escrowUtxoOutput.amount[0].quantity))
        setAssetCount(escrowUtxoOutput.amount.length - 1)
        setAssets((p) => {
          let newAssets = [...escrowUtxoOutput.amount.slice(1)]
          return newAssets
        })
      } else if (noEscrowContractInteraction) {
        const userInputs = transaction.inputs.filter(
          (input) => input.address === userAddr
        )
        const userOutputs = transaction.outputs.filter((out) => out.address === userAddr)

        if (userInputs.length) {
          setType("out")
          const amountIn = reduceTxAmount(userInputs)
          const changeAmount = reduceTxAmount(userOutputs)

          const difference = {}
          for (let k of Object.keys(changeAmount)) {
            difference[k] = amountIn[k] - changeAmount[k]
          }
          setLovelace(difference["lovelace"])
          let newAssets = Object.keys(difference)
            .filter((val) => val !== "lovelace")
            .map((k) => ({ unit: k, quantity: difference[k] }))
          setAssetCount(newAssets.length)
          setAssets(newAssets)
        } else {
          setType("in")
          const receivingAmount = userOutputs.reduce((p, acc) => {
            for (let [_, { quantity, unit }] of acc.amount.entries()) {
              p[unit] = Number(p[unit] || 0) + Number(quantity)
            }
            return p
          }, {})
          setLovelace(receivingAmount["lovelace"])
          let newAssets = Object.keys(receivingAmount)
            .filter((val) => val !== "lovelace")
            .map((k) => ({ unit: k, quantity: receivingAmount[k] }))
          setAssetCount(newAssets.length - 1)
          setAssets(newAssets)
        }
      }
    }, [transaction])

    return (
      <Pressable
        style={Buttons.applyOpacity(styles.container)}
        hitSlop={Sizing.x15}
        onPress={onTxItemPress}>
        {type === "out" ? (
          <UpArrow width={22} height={22} stroke={Colors.danger.s400} strokeWidth={2} />
        ) : type === "in" ? (
          <DownArrow
            width={22}
            height={22}
            stroke={Colors.success.s400}
            strokeWidth={2}
          />
        ) : (
          <ArrowUpDown
            width={22}
            height={22}
            stroke={
              colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral
            }
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
            {`${lovelaceToAda(BigInt(lovelace || 0))} ADA ${
              assetCount > 0
                ? `+ ${assetCount} ${assetCount > 1 ? "Assets" : "Asset"}`
                : ""
            }`}
          </SubHeaderText>
        </View>
        <RightArrowIcon
          width="20"
          height="20"
          strokeWidth={2}
          stroke={colorScheme === "light" ? Colors.primary.s800 : Colors.primary.neutral}
          style={{ marginRight: "auto" }}
        />
      </Pressable>
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
