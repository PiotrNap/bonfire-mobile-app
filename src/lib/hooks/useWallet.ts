import * as React from "react"
import { Toast } from "react-native-toast-message/lib/src/Toast"
import { Crypto, TxInput } from "@hyperionbt/helios"
import { BlockFrostDetailedTx } from "lib/wallet/types"
import { showErrorToast } from "lib/helpers"
import { TX_GET_SIZE, Wallet } from "lib/wallet"
import {
  assetsToUnitsArray,
  COLLATERAL_STORAGE_KEY,
  filterLovelaceOnlyInputs,
  lovelaceValueOfInputs,
  txInputsToAssets,
} from "lib/wallet/utils"
import { useFocusEffect } from "@react-navigation/native"
import AsyncStorage from "@react-native-async-storage/async-storage"
import { walletContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"

export const useWallet = (makeInitialFetch = true) => {
  const {
    txHistory,
    setTxHistory,
    baseAddress,
    setLovelaceBalance,
    setWalletUtxos,
    setWalletAssets,
    setBaseAddress,
  } = walletContext()
  const { collateralUtxoId } = React.useContext(ProfileContext)
  const [isPaginationLoading, setIsPaginationLoading] = React.useState<boolean>(false)
  const [lockedLovelaceBalance, setLockedLovelaceBalance] = React.useState<bigint>(0n)

  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [txListPage, setTxListPage] = React.useState<number>(1)
  const [txHistoryEndReached, setTxHistoryEndReached] = React.useState<boolean>(false)
  // const [aU, setaU] = React.useState<any>(1)
  // const [tU, settU] = React.useState<any>(1)

  /** Update wallet Utxos and ADA balance **/
  useFocusEffect(
    React.useCallback(() => {
      if (makeInitialFetch)
        (async () => {
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

  const updateWalletBalance = React.useCallback(
    async (addr?: string) => {
      try {
        // console.log("tokens update :", aU)
        // setaU((p) => p + 1)

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
        const collateralUtxo =
          collateralUtxoId &&
          (data as TxInput[]).some(
            (txInput) =>
              `${txInput.outputId.txId}#${txInput.outputId.utxoIdx}` === collateralUtxoId
          )

        // user doesn't have the collateral any more
        if (!collateralUtxo && collateralUtxoId) {
          await AsyncStorage.removeItem(COLLATERAL_STORAGE_KEY)
        }

        const availableLovelace = lovelaceValueOfInputs(filterLovelaceOnlyInputs(data))
        const lockedLovelace = lovelaceValueOfInputs(data) - availableLovelace
        //@TODO implement proper calculation of locked lovelaces
        setLovelaceBalance(availableLovelace + lockedLovelace)
        setLockedLovelaceBalance(lockedLovelace)

        // extracts assets from TxInputs into a Map
        const assets = assetsToUnitsArray(txInputsToAssets(data))
        setWalletAssets(new Map(assets))

        setWalletUtxos(data)
        setIsLoading(false)
      } catch (e) {
        showErrorToast(e)
      }
    },
    [baseAddress, collateralUtxoId]
  )
  const updateWalletTxHistory = React.useCallback(
    async (addr?: string, refresh = true) => {
      try {
        if (!refresh) {
          if (txHistoryEndReached) return // no more pagination
          setIsPaginationLoading(true)
        }

        // console.log("txs update :", tU)
        // settU((p) => p + 1)

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
        // fullInfoTxs = fullInfoTxs.reverse() // because Blockfrost can't sort them
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
    [baseAddress, txHistory, txListPage, txHistoryEndReached]
  )

  return {
    isLoading,
    isPaginationLoading,
    updateWalletBalance,
    updateWalletTxHistory,
    lockedLovelaceBalance,
  }
}
