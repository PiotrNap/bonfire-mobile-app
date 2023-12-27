import {
  Address,
  BlockfrostV0,
  NetworkParams,
  Tx,
  TxInput,
  TxOutput,
  Value,
  Bip32PrivateKey,
  hexToBytes,
  bytesToHex,
  Datum,
} from "@hyperionbt/helios"
import { mnemonicToEntropy } from "bip39"
import * as CrossCSL from "@emurgo/cross-csl-mobile"
import {
  BLOCKFROST_API_KEY_MAINNET,
  BLOCKFROST_API_KEY_TESTNET,
  TREASURY_ADDRESS,
} from "@env"

//@ts-ignore
const { Bip32PublicKey } = CrossCSL

import { CardanoMobile } from "../../../global"
import { ROLE_TYPE, CONFIG_NUMBERS } from "./config"
import {
  EscrowContractDatum,
  NetworkId,
  PromiseHandlerRes,
  SendRegularTxInfo,
  TxHash,
  WalletKeys,
} from "./types"
import { COLLATERAL_LOVELACE, COLLATERAL_STORAGE_KEY, unitsToAssets } from "./utils"
import { mainnet } from "../../on_chain/configs/mainnet"
import { preprod } from "../../on_chain/configs/preprod"
import {
  escrowProgram,
  escrowProgramCompiled,
  escrowValidatorHash,
} from "../../on_chain/EscrowContract"
import AsyncStorage from "@react-native-async-storage/async-storage"

export const TX_GET_SIZE = 20
export const PURPOSE = 2147485500
export const COIN_TYPE = 2147485463
export const HARD_DERIVATION_START = 2147483648

export function getConfigForNetworkId(networkId: NetworkId) {
  const networkName = networkId === "Mainnet" ? "mainnet" : "preprod"
  const blockfrostApiKey =
    networkId === "Mainnet" ? BLOCKFROST_API_KEY_MAINNET : BLOCKFROST_API_KEY_TESTNET
  const networkConfig = networkId === "Mainnet" ? mainnet : preprod

  return { networkName, blockfrostApiKey, networkConfig }
}

export async function blockFrostFetch(endpoint: string, networkId: NetworkId) {
  const { networkName, blockfrostApiKey } = getConfigForNetworkId(networkId)
  if (endpoint && typeof endpoint === "string") {
    try {
      const response = await fetch(
        `https://cardano-${networkName}.blockfrost.io/api/v0/${endpoint}`,
        {
          method: "GET",
          headers: {
            project_id: blockfrostApiKey,
          },
        }
      )

      return await response.json()
    } catch (e) {
      throw e
    }
  } else throw new Error(`Unrecognized endpoint: ${endpoint}`)
}

export function blockFrost(networkId: NetworkId) {
  const { networkName, blockfrostApiKey } = getConfigForNetworkId(networkId)
  return new BlockfrostV0(networkName, blockfrostApiKey)
}

export class Wallet {
  /**
   * Creates root key and account keys with base address
   */
  async init(mnemonic: string | undefined): Promise<WalletKeys | void> {
    if (!mnemonic) throw new Error(`Missing mnemonic in Wallet.init`)

    try {
      const rootKeyPtr = await this.createRootKeyPtr(mnemonic)
      const rootKeyHex: string = await this.createRootKey(rootKeyPtr)
      const { accountKeyHex, accountPubKeyHex, accountPubKeyPtr } =
        await this.createAccountKeys(0, rootKeyPtr)

      // for first release use first index only
      const mainnetBaseAddress = await this.generateBaseAddress(accountPubKeyPtr, 0, "1")
      const testnetBaseAddress = await this.generateBaseAddress(accountPubKeyPtr, 0, "0")
      if (!mainnetBaseAddress || !testnetBaseAddress)
        throw new Error(`Missing baseAddress pointer`)

      return {
        addresses: {
          mainnet: mainnetBaseAddress,
          testnet: testnetBaseAddress,
        },
        rootKeyHex,
        accountPubKeyHex,
        accountKeyHex,
      }
    } catch (e) {
      console.error(e)
      throw e
    }
  }

  async generateBaseAddress(
    accountPubKey: typeof Bip32PublicKey,
    index: number,
    networkMagic: "1" | "0"
  ): Promise<string> {
    const chainVKey = await accountPubKey
      .derive(ROLE_TYPE.EXTERNAL_CHAIN)
      .then((key) => key.derive(index))
      .then((key) => key.toRawKey())

    const stakingVKey = await accountPubKey
      .derive(ROLE_TYPE.STAKING_KEY)
      .then((key) => key.derive(CONFIG_NUMBERS.STAKING_ACCOUNT_INDEX))
      .then((key) => key.toRawKey())

    const addr = await CardanoMobile.BaseAddress.new(
      parseInt(networkMagic, 10),
      await CardanoMobile.Credential.fromKeyhash(await chainVKey.hash()),
      await CardanoMobile.Credential.fromKeyhash(await stakingVKey.hash())
    )

    return (await addr.toAddress()).toBech32()
  }

  async createRootKeyPtr(mnemonic: string): Promise<any> {
    const bip39entropy = mnemonicToEntropy(mnemonic)
    const EMPTY_PASSWORD = Buffer.from("")
    const rootKeyPtr = await CardanoMobile.Bip32PrivateKey.fromBip39Entropy(
      Buffer.from(bip39entropy, "hex"),
      EMPTY_PASSWORD
    )
    return rootKeyPtr
  }

  async createRootKey(rootKeyPtr: any): Promise<string> {
    return Buffer.from(await rootKeyPtr.asBytes()).toString("hex")
  }

  async createAccountKeys(acctIndex = 0, rootKeyPtr: any) {
    const accountKeyPtr = await rootKeyPtr
      .derive(PURPOSE)
      .then((key) => key.derive(COIN_TYPE))
      .then((key) => key.derive(acctIndex + HARD_DERIVATION_START))

    const accountKeyHex = await accountKeyPtr
      .asBytes()
      .then((bytes) => Buffer.from(bytes).toString("hex"))

    const accountPubKeyPtr = await rootKeyPtr
      .derive(PURPOSE)
      .then((key) => key.derive(COIN_TYPE))
      .then((key) => key.derive(acctIndex + HARD_DERIVATION_START))
      .then((accountKey) => accountKey.toPublic())
    const accountPubKeyHex = await accountPubKeyPtr
      .asBytes()
      .then((bytes) => Buffer.from(bytes).toString("hex"))

    return {
      accountKeyPtr,
      accountPubKeyPtr,
      accountKeyHex,
      accountPubKeyHex,
    }
  }

  static async sendRegularTransaction(
    { assets, receiverAddress, lovelace }: SendRegularTxInfo,
    userAddress: string,
    userUtxos: TxInput[],
    signingKey: string,
    isCollateralSplitTx: boolean = false,
    networkId: NetworkId
  ): Promise<TxHash | void> {
    // console.log(
    //   "regular tx args >",
    //   assets,
    //   receiverAddress,
    //   lovelace,
    //   userAddress,
    //   userUtxos,
    //   signingKey
    // )
    const privKey = new Bip32PrivateKey(hexToBytes(signingKey))
    const { networkConfig } = getConfigForNetworkId(networkId)
    const params = new NetworkParams(networkConfig)

    const outputAddress = new Address(receiverAddress)
    const outputAssets = unitsToAssets(assets)

    const outputValue = new Value(lovelace, outputAssets)
    const output = new TxOutput(outputAddress, outputValue)
    const now = Date.now()
    const fiveMinutes = 1000 * 60 * 5

    const collateralUtxoId = await AsyncStorage.getItem(COLLATERAL_STORAGE_KEY)
    // prevent spending the collateral utxo
    if (collateralUtxoId && !isCollateralSplitTx) {
      userUtxos = userUtxos.filter(
        (txIn) => `${txIn.outputId.txId}#${txIn.outputId.utxoIdx}` !== collateralUtxoId
      )
    }

    const tx = new Tx()
      .addInputs(userUtxos)
      .addOutput(output)
      .validFrom(new Date(now - fiveMinutes))
      .validTo(new Date(now + fiveMinutes))

    try {
      await tx.finalize(params, new Address(userAddress))
      let signature = privKey.sign(tx.bodyHash)
      tx.addSignature(signature)

      const { data, error } = await Wallet.submitTransaction(tx, networkId)
      if (error) throw error

      const txHash = bytesToHex(data.bytes)
      // update collateral utxoId
      if (isCollateralSplitTx) {
        const newCollateralIdx = tx.body.outputs.findIndex(
          (output) =>
            output.value.assets.isZero() && output.value.lovelace === COLLATERAL_LOVELACE
        )
        await AsyncStorage.setItem(
          COLLATERAL_STORAGE_KEY,
          `${txHash}#${newCollateralIdx}`
        )
      }

      return txHash
    } catch (e) {
      throw e
    } finally {
      signingKey = ""
    }
  }

  static async sendLockingTransaction(
    paymentTokens: Value,
    lockingDatumInfo: EscrowContractDatum,
    userAddress: string,
    userUtxos: TxInput[],
    signingKey: string,
    networkId: NetworkId
  ) {
    const privKey = new Bip32PrivateKey(hexToBytes(signingKey))
    const { networkConfig } = getConfigForNetworkId(networkId)
    const params = new NetworkParams(networkConfig)
    const now = Date.now()
    const fiveMinutes = 1000 * 60 * 5

    const escrowDatum = new escrowProgram.types.Datum(...Object.values(lockingDatumInfo))
    const inlineDatum = Datum.inline(escrowDatum)

    const lockingTxOutput = new TxOutput(
      Address.fromHash(escrowValidatorHash),
      paymentTokens,
      inlineDatum
    )
    // - collateral utxo needs to be updated since it will be spent
    // - prevent spending the collateral in other type transactions

    const collateralUtxoId = await AsyncStorage.getItem(COLLATERAL_STORAGE_KEY)
    // prevent spending the collateral utxo
    if (collateralUtxoId) {
      userUtxos = userUtxos.filter(
        (txIn) => `${txIn.outputId.txId}#${txIn.outputId.utxoIdx}` !== collateralUtxoId
      )
    }

    // lock funds
    const lockingTx = new Tx()
      .addInputs(userUtxos)
      .addOutput(lockingTxOutput)
      .validFrom(new Date(now - fiveMinutes))
      .validTo(new Date(now + fiveMinutes))

    try {
      await lockingTx.finalize(params, new Address(userAddress))
      let signature = privKey.sign(lockingTx.bodyHash)
      lockingTx.addSignature(signature)

      const { data, error } = await Wallet.submitTransaction(lockingTx, networkId)
      if (error) throw error

      return { txHash: bytesToHex(data.bytes), datumHash: inlineDatum.hash.hex }
    } catch (e) {
      throw e
    } finally {
      signingKey = ""
    }
  }

  static async sendPayoutTransaction(
    unlockingTxInput: TxInput,
    spareUtxos: TxInput[],
    collateralUtxoIn: TxInput,
    feeUtxo: TxInput,
    userWalletAddress: string,
    signingKey: string,
    serviceFee: BigInt,
    hasBetaTesterToken: boolean,
    networkId: NetworkId
  ) {
    const now = Date.now()
    const fiveMinutes = 1000 * 60 * 5
    const { networkConfig } = getConfigForNetworkId(networkId)
    const params = new NetworkParams(networkConfig)
    const privKey = new Bip32PrivateKey(hexToBytes(signingKey))
    const treasuryAddress = new Address(TREASURY_ADDRESS)
    const userAddress = Address.fromBech32(userWalletAddress)
    const userPubKeyHash = userAddress.pubKeyHash
    if (!userPubKeyHash)
      throw Error("Unable to obtain the PubKey hash for user wallet address")

    const frstTxOutId = new escrowProgram.types.TxOutId(
      unlockingTxInput.outputId.txId.hex,
      unlockingTxInput.outputId.utxoIdx
    )
    const redeemer = new escrowProgram.types.Redeemer.Complete([frstTxOutId])
    const collateralUtxoOut = new TxOutput(userAddress, collateralUtxoIn.value)

    //@TODO after beta release add script reference
    /** For wallets with Beta-Tester NFT we don't expect any fee to be sent to the treasury **/
    const unlockingTx = new Tx()
      .attachScript(escrowProgramCompiled)
      .addInputs([...spareUtxos, feeUtxo])
      .addCollateral(collateralUtxoIn)
      .addInput(unlockingTxInput, redeemer)
      .addOutput(collateralUtxoOut) // produce a fresh collateral utxo
      .addSigner(userPubKeyHash)
      .validFrom(new Date(now - fiveMinutes))
      .validTo(new Date(now + fiveMinutes))

    if (!hasBetaTesterToken) {
      const serviceFeeTxOut = new TxOutput(treasuryAddress, new Value(Number(serviceFee)))
      unlockingTx.addOutput(serviceFeeTxOut)
    }

    try {
      await unlockingTx.finalize(params, new Address(userWalletAddress))
      let signature = privKey.sign(unlockingTx.bodyHash)
      unlockingTx.addSignature(signature)

      // console.log(JSON.stringify(unlockingTx.body.dump(), null, 4))

      const { data, error } = await Wallet.submitTransaction(unlockingTx, networkId)
      if (error) throw error

      const txHash = bytesToHex(data.bytes)
      // update collateral utxoId
      const newCollateralIdx = unlockingTx.body.outputs.findIndex(
        (output) =>
          output.value.assets.isZero() && output.value.lovelace === COLLATERAL_LOVELACE
      )
      await AsyncStorage.setItem(COLLATERAL_STORAGE_KEY, `${txHash}#${newCollateralIdx}`)

      return { txHash }
    } catch (e) {
      throw e
    } finally {
      signingKey = ""
    }
  }
  static async sendCancellationTransaction(
    unlockingTxInput: TxInput,
    spareUtxos: TxInput[],
    collateralUtxoIn: TxInput,
    feeUtxo: TxInput,
    cancellationFeeValue: Value, // based on beneficiary's event cancellation rate
    beneficiaryAddress: string,
    benefactorAddress: string,
    signingKey: string,
    isBeneficiary: boolean, // whether the user initiating tx is the organizer
    isBeforeCancellationWindow: boolean,
    networkId: NetworkId
  ) {
    const now = Date.now()
    const fiveMinutes = 1000 * 60 * 5
    const { networkConfig } = getConfigForNetworkId(networkId)
    const params = new NetworkParams(networkConfig)
    const privKey = new Bip32PrivateKey(hexToBytes(signingKey))
    const userAddress = Address.fromBech32(
      isBeneficiary ? beneficiaryAddress : benefactorAddress
    )
    const userPubKeyHash = userAddress.pubKeyHash
    if (!userPubKeyHash)
      throw Error("Unable to obtain the PubKey hash for user wallet address")

    const redeemer = new escrowProgram.types.Redeemer.Cancel(
      unlockingTxInput.outputId.txId.hex,
      unlockingTxInput.outputId.utxoIdx
    )
    const collateralUtxoOut = new TxOutput(userAddress, collateralUtxoIn.value)

    //@TODO after beta release add script reference
    const unlockingTx = new Tx()
      .attachScript(escrowProgramCompiled)
      .addInputs([...spareUtxos, feeUtxo])
      .addCollateral(collateralUtxoIn)
      .addInput(unlockingTxInput, redeemer)
      .addOutput(collateralUtxoOut) // produce a fresh collateral utxo
      .addSigner(userPubKeyHash)
      .validFrom(new Date(now - fiveMinutes))
      .validTo(new Date(now + fiveMinutes))

    /*
     * if benefactor is cancelling before cancellation window there's nothing to be added to the output
     */
    if (isBeneficiary) {
      // return everything to the benefactor
      unlockingTx.addOutput(
        new TxOutput(Address.fromBech32(benefactorAddress), unlockingTxInput.value)
      )
    } else {
      // means it's during cancellation window
      if (!isBeforeCancellationWindow) {
        unlockingTx.addOutput(
          new TxOutput(Address.fromBech32(beneficiaryAddress), cancellationFeeValue)
        )
        unlockingTx.addOutput(
          new TxOutput(
            Address.fromBech32(benefactorAddress),
            unlockingTxInput.value.sub(cancellationFeeValue)
          )
        )
      }
    }
    console.log(JSON.stringify(unlockingTx.body.dump(), null, 4))

    try {
      await unlockingTx.finalize(params, userAddress)
      let signature = privKey.sign(unlockingTx.bodyHash)
      unlockingTx.addSignature(signature)

      // console.log(JSON.stringify(unlockingTx.body.dump(), null, 4))

      const { data, error } = await Wallet.submitTransaction(unlockingTx, networkId)
      if (error) throw error

      const txHash = bytesToHex(data.bytes)
      // update collateral utxoId
      const newCollateralIdx = unlockingTx.body.outputs.findIndex(
        (output) =>
          output.value.assets.isZero() && output.value.lovelace === COLLATERAL_LOVELACE
      )
      await AsyncStorage.setItem(COLLATERAL_STORAGE_KEY, `${txHash}#${newCollateralIdx}`)

      return { txHash }
    } catch (e) {
      throw e
    } finally {
      signingKey = ""
    }
  }

  /** Get wallet tx's **/
  static async getTransactionsAtAddress(
    address: string,
    page: number = 0,
    networkId: NetworkId
  ): Promise<PromiseHandlerRes> {
    return Wallet.promiseHandler(
      blockFrostFetch(
        `addresses/${address}/transactions?page=${page}&count=${TX_GET_SIZE}&order=desc`,
        networkId
      )
    )
  }
  static async submitTransaction(
    tx: Tx,
    networkId: NetworkId
  ): Promise<PromiseHandlerRes> {
    return Wallet.promiseHandler(blockFrost(networkId).submitTx(tx))
  }

  /** Get wallet balance **/
  static async getUtxosAtAddress(
    addr: string,
    networkId: NetworkId
  ): Promise<PromiseHandlerRes> {
    return Wallet.promiseHandler(blockFrost(networkId).getUtxos(new Address(addr)))
  }

  static async getAssetInfo(
    unit: string,
    networkId: NetworkId
  ): Promise<PromiseHandlerRes> {
    return Wallet.promiseHandler(blockFrostFetch(`/assets/${unit}`, networkId))
  }

  /** Get detailed Tx info **/
  static async getTxUtxos(
    txHash: string,
    networkId: NetworkId
  ): Promise<PromiseHandlerRes> {
    return Wallet.promiseHandler(blockFrostFetch(`/txs/${txHash}/utxos`, networkId))
  }

  static async promiseHandler(promise: any): Promise<PromiseHandlerRes> {
    const handlerRes: PromiseHandlerRes = {
      error: "",
      data: null,
      statusCode: 0,
    }
    try {
      const res = await promise

      if (res.json) {
        handlerRes.data = await res.json()
      } else if (res.data) {
        handlerRes.data = res.data
      } else if (res.error) {
        handlerRes.error = res.error
        handlerRes.statusCode = res.status_code || res.status || null
      } else handlerRes.data = res
    } catch (e) {
      handlerRes.error = e.message || e
      handlerRes.statusCode = e.status
    }
    return handlerRes
  }
}
