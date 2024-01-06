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
import { appContext, walletContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"

// TODO
// 1. reduce unnecessary re-fetch of the already fetched Tx-Info
//      ... see "for (let transaction of transactions) {"

export const useWallet = (makeInitialFetch = true) => {
  const {
    txHistory,
    setTxHistory,
    addresses,
    setLovelaceBalance,
    setWalletUtxos,
    setWalletAssets,
    setBaseAddresses,
  } = walletContext()
  const { networkId } = appContext()
  const { collateralUtxoId } = React.useContext(ProfileContext)
  const [isPaginationLoading, setIsPaginationLoading] = React.useState<boolean>(false)
  const [lockedLovelaceBalance, setLockedLovelaceBalance] = React.useState<bigint>(0n)

  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [txListPage, setTxListPage] = React.useState<number>(1)
  const [txHistoryEndReached, setTxHistoryEndReached] = React.useState<boolean>(false)

  const txPromiseRef = React.useRef<Promise<void> | null>(null)
  const walletBalancePromiseRef = React.useRef<Promise<void> | null>(null)
  const networkBasedAddress =
    networkId === "Mainnet" ? addresses.mainnet : addresses.testnet

  /** Update wallet Utxos and ADA balance **/
  useFocusEffect(
    React.useCallback(() => {
      if (makeInitialFetch) {
        ;(async () => {
          if (!addresses.mainnet || !addresses.testnet) {
            let addresses: any = await AsyncStorage.getItem("account-#0-baseAddresses")
            if (typeof addresses === "string" && addresses) {
              addresses = JSON.parse(addresses)
              setBaseAddresses(addresses)
              updateWalletBalance(
                networkId === "Mainnet" ? addresses.mainnet : addresses.testnet
              )
              updateWalletTxHistory(
                networkId === "Mainnet" ? addresses.mainnet : addresses.testnet
              )
            }
          } else {
            updateWalletBalance(networkBasedAddress)
            updateWalletTxHistory(networkBasedAddress)
          }
        })()
      }
    }, [networkId, networkBasedAddress])
  )

  const updateWalletBalance = (addr?: string) => {
    addr = addr ?? networkBasedAddress
    if (walletBalancePromiseRef.current) return walletBalancePromiseRef.current

    const newPromise = getWalletBalance(addr)
    walletBalancePromiseRef.current = newPromise
    return newPromise
  }

  const getWalletBalance = React.useCallback(
    async (addr?: string) => {
      try {
        setIsLoading(true)
        addr = addr ?? networkBasedAddress

        const isGoodAddr = Crypto.verifyBech32(addr)
        if (!isGoodAddr)
          Toast.show({
            type: "error",
            text1: "Unexpected address",
            text2: "Cannot fetch address assets.",
          })

        const { data, error } = await Wallet.getUtxosAtAddress(addr, networkId)
        if (error)
          Toast.show({
            type: "error",
            text1: error || "Unable to get assets",
            text2: "Maybe try again?",
          })
        if (!data?.length) {
          setLovelaceBalance(0n)
          setLockedLovelaceBalance(0n)
          setWalletAssets(new Map(data))
          setWalletUtxos(data)

          return
        }
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
        //@TODO after release; implement proper calculation of locked lovelaces
        setLovelaceBalance(availableLovelace + lockedLovelace)
        setLockedLovelaceBalance(lockedLovelace)

        // extracts assets from TxInputs into a Map
        const assets = assetsToUnitsArray(txInputsToAssets(data))
        setWalletAssets(new Map(assets))

        setWalletUtxos(data)
      } catch (e) {
        showErrorToast(e)
      } finally {
        setIsLoading(false)
        walletBalancePromiseRef.current = null
      }
    },
    [networkBasedAddress, collateralUtxoId, networkId]
  )

  const updateWalletTxHistory = (addr?: string, refresh = true) => {
    addr = addr ?? networkBasedAddress
    if (txPromiseRef.current) return txPromiseRef.current

    const newPromise = getTxHistory(addr, refresh)
    txPromiseRef.current = newPromise
    return newPromise
  }

  const getTxHistory = React.useCallback(
    async (addr: string, refresh) => {
      try {
        if (!refresh) {
          if (txHistoryEndReached) return // no more pagination
          setIsPaginationLoading(true)
        }

        const isGoodAddr = Crypto.verifyBech32(addr)
        if (!isGoodAddr)
          Toast.show({
            type: "error",
            text1: "Unexpected address",
            text2: "Cannot fetch address transactions.",
          })

        const {
          data: transactions,
          error,
          statusCode,
        } = await Wallet.getTransactionsAtAddress(
          addr,
          refresh ? 1 : txListPage,
          networkId
        )
        if (statusCode === 404 || !transactions?.length) {
          setTxHistory(transactions)
          return
        }
        if (error) {
          Toast.show({
            type: "error",
            text1: error || "Unable to get transactions",
            text2: "Maybe try again?",
          })
        }
        if (transactions.length < TX_GET_SIZE) {
          setTxHistoryEndReached(true) // we've reached the end
        }

        let fullInfoTxs: BlockFrostDetailedTx[] = []
        console.log("txHistory >", txHistory)
        console.log("transaction >", transactions)
        // there's probably a better way to fetch every tx utxos
        for (let transaction of transactions) {
          const oldFullTxInfo = txHistory?.find(
            (tx) =>
              tx.hash === transaction.tx_hash && tx.block_time === transaction.block_time
          )
          console.log(transaction)
          console.log("exists ??? >", !!oldFullTxInfo)
          if (oldFullTxInfo) {
            fullInfoTxs.push(transaction)
            continue
          }

          const { data: tx, error } = await Wallet.getTxUtxos(
            transaction?.tx_hash,
            networkId
          )

          if (error)
            Toast.show({
              type: "error",
              text1: error || "Unable to get transaction info",
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
      } catch (e) {
        showErrorToast(e)
      } finally {
        setIsPaginationLoading(false)
        txPromiseRef.current = null
      }
    },
    [networkBasedAddress, txHistory, txListPage, txHistoryEndReached, networkId]
  )

  return {
    isLoading,
    isPaginationLoading,
    updateWalletBalance,
    updateWalletTxHistory,
    lockedLovelaceBalance,
  }
}
