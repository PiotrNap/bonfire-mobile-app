import { randomBytes } from "react-native-randombytes"
import { Address, BlockfrostV0, Tx } from "@hyperionbt/helios"
import { generateMnemonic, mnemonicToEntropy } from "bip39"
import * as CrossCSL from "@emurgo/cross-csl-mobile"
import { CARDANO_NETWORK, BLOCKFROST_API_KEY } from "@env"
import axios from "axios"

//@ts-ignore
const { Bip32PrivateKey, Bip32PublicKey } = CrossCSL

import { CardanoMobile } from "../../../global"
import {
  ROLE_TYPE,
  CONFIG_NUMBERS,
  HASKELL_SHELLEY,
  HASKELL_SHELLEY_TESTNET,
  NetworkConfig,
} from "./config"
import { WalletKeys } from "./types"

export const TX_GET_SIZE = 30
export const PURPOSE = 2147485500
export const COIN_TYPE = 2147485463
export const HARD_DERIVATION_START = 2147483648

export async function blockFrostFetch(endpoint: string) {
  const networkName = CARDANO_NETWORK
  const blockfrostApiKey = BLOCKFROST_API_KEY

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

export function blockFrost() {
  return new BlockfrostV0(CARDANO_NETWORK, BLOCKFROST_API_KEY)
}

export class Wallet {
  chainConfig: NetworkConfig

  // @TODO make sure all parameters are set correctly
  constructor() {
    this.chainConfig =
      CARDANO_NETWORK === "mainnet" ? HASKELL_SHELLEY : HASKELL_SHELLEY_TESTNET
  }

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
      const baseAddress = await this.generateBaseAddress(accountPubKeyPtr, 0)
      if (!baseAddress) throw new Error(`Undefined - baseAddressPtr`)

      return {
        baseAddress,
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
    index: number
  ): Promise<string> {
    const chainVKey = await accountPubKey
      .derive(ROLE_TYPE.EXTERNAL_CHAIN)
      .then((key) => key.derive(index))
      .then((key) => key.toRawKey())

    const stakingVKey = await accountPubKey
      .derive(ROLE_TYPE.STAKING_KEY)
      .then((key) => key.derive(CONFIG_NUMBERS.STAKING_ACCOUNT_INDEX))
      .then((key) => key.toRawKey())

    console.log(JSON.stringify(CardanoMobile, null, 4))

    const addr = await CardanoMobile.BaseAddress.new(
      parseInt(this.chainConfig.CHAIN_NETWORK_ID, 10),
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

  static async generateADAMnemonic(): Promise<undefined | string> {
    return new Promise((res, rej) => {
      let mnemonic = generateMnemonic(160, randomBytes)
      if (!mnemonic) rej()
      res(mnemonic)
    })
  }

  async signTransaction() {}

  /** Get wallet tx's **/
  static async getTransactionsAtAddress(address: string, page: number = 0) {
    return Wallet.promiseHandler(
      blockFrostFetch(
        `/addresses/${address}/transactions?page=${page}&count=${TX_GET_SIZE}`
      )
    )
  }
  static async submitTransaction(tx: Tx) {
    return Wallet.promiseHandler(blockFrost().submitTx(tx))
  }

  /** Get wallet balance **/
  static async getUtxosAtAddress(addr: string) {
    return Wallet.promiseHandler(blockFrost().getUtxos(new Address(addr)))
  }

  static async getAssetInfo(unit: string) {
    return Wallet.promiseHandler(blockFrostFetch(`/assets/${unit}`))
  }

  /** Get detailed Tx info **/
  static async getTxUtxos(txHash: string) {
    return Wallet.promiseHandler(blockFrostFetch(`/txs/${txHash}/utxos`))
  }

  static async getNetworkParams() {
    return Wallet.promiseHandler(
      fetch(`https://d1t0d7c2nekuk0.cloudfront.net/${process.env.CARDANO_NETWORK}.json`)
    )
  }

  static async validatePlutusScriptOffchain(
    // txCbor: number[], // cbor
    // txInputs: any[],
    // escrowProgramCbor: number[], // cbor of compiled Uplc program
    userID: string
  ) {
    try {
      const res = await axios.get(`users/${userID}/tx/plutus-scripts`)
      // txCbor,
      // txInputs,
      // escrowProgramCbor,
      // })
      return res.data
    } catch (e) {
      console.error(e.data)
      throw e
    }
  }

  static async promiseHandler(promise: any) {
    let error
    let data
    try {
      const res = await promise
      // console.log("res ?", JSON.stringify(res, null, 4))
      if (res.json) {
        data = await res.json()
      } else if (res.data) {
        data = res.data
      } else if (res.error) {
        error = res.error
      } else data = res
    } catch (e) {
      console.error(e)
      error = e
    }
    return { error, data }
  }
}
