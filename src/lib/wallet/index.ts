import { generateMnemonic } from "bip39"
import axios from "../../services/Api/base"

import {
  getFromEncryptedStorage,
  setToEncryptedStorage,
  StoragePropertyKeys,
} from "../encryptedStorage"
import { decryptWithPassword, encryptWithPassword } from "../helpers"
import {
  ROLE_TYPE,
  CONFIG_NUMBERS,
  HASKELL_SHELLEY,
  HASKELL_SHELLEY_TESTNET,
  NetworkConfig,
} from "./config"
import { CARDANO_NETWORK, BLOCKFROST_KEY } from "@env"
import { WalletKeys } from "./types"
import { randomBytes } from "react-native-randombytes"
import {
  BlockfrostV0,
  Address,
  TxInput,
  RootPrivateKey,
  bytesToHex,
  Bip32PrivateKey,
  Assets,
} from "@hyperionbt/helios"
import { fromAssetUnit } from "./utils"

export const WALLET_IMPLEMENTATION_ID = "haskell-shelley"
export const DISCOVERY_GAP_SIZE = 20
export const DISCOVERY_BLOCK_SIZE = 50 // should be less than API limitations
export const MAX_GENERATED_UNUSED = 20 // must be <= gap size
export const PURPOSE = 2147485500
export const COIN_TYPE = 2147485463
export const ACCOUNT_INDEX = 0
export const HARD_DERIVATION_START = 2147483648
export const CHIMERIC_ACCOUNT = 2
export const STAKING_KEY_INDEX = 0

export class Wallet {
  chainConfig: NetworkConfig

  // @TODO make sure all parameters are set correctly
  constructor() {
    this.chainConfig =
      CARDANO_NETWORK === "mainnet" ? HASKELL_SHELLEY : HASKELL_SHELLEY_TESTNET
  }

  get BlockfrostAPI() {
    //@ts-ignore
    return new BlockfrostV0(CARDANO_NETWORK, BLOCKFROST_KEY)
  }

  /**
   * Creates root key and account keys with base address
   */
  init(mnemonic: string | undefined): WalletKeys {
    if (!mnemonic) throw new Error(`Missing mnemonic in Wallet.init`)

    const rootKey = RootPrivateKey.fromPhrase(mnemonic.split(" "))
    const rootKeyHex: string = bytesToHex(rootKey.bytes)
    const { accountKey, accountPubKey } = this.createAccountKeys(0, rootKey)

    // for first release use first index only
    const baseAddress = this.generateBaseAddress(accountKey, 0)
    if (!baseAddress) throw new Error(`Undefined - baseAddress`)

    return {
      baseAddress: baseAddress.toBech32(),
      rootKeyHex,
      accountPubKeyHex: accountPubKey.hex,
      accountKeyHex: bytesToHex(accountKey.bytes),
    }
  }

  static async generateADAMnemonic(): Promise<undefined | string> {
    return new Promise((res, rej) => {
      let mnemonic = generateMnemonic(160, randomBytes)
      if (!mnemonic) rej()
      res(mnemonic)
    })
  }

  generateBaseAddress(accountKey: Bip32PrivateKey, index: number): Address {
    const chainKey = accountKey
      .derive(ROLE_TYPE.EXTERNAL_CHAIN)
      .derive(index)
      .derivePubKey()
    const stakeKey = accountKey
      .derive(ROLE_TYPE.STAKING_KEY)
      .derive(CONFIG_NUMBERS.STAKING_ACCOUNT_INDEX)
      .derivePubKey()

    const baseAddress = Address.fromHashes(chainKey.pubKeyHash, stakeKey.pubKeyHash)

    return baseAddress
  }

  createAccountKeys(acctIndex = 0, rootKeyPtr: RootPrivateKey) {
    const acctKey = rootKeyPtr
      .derive(PURPOSE)
      .derive(COIN_TYPE)
      .derive(acctIndex + HARD_DERIVATION_START)
    const acctPubKey = acctKey.derivePubKey()

    return {
      accountKey: acctKey,
      accountPubKey: acctPubKey,
    }
  }

  async encryptAndStoreOnDevice(
    value: string,
    password: string,
    storageKey: StoragePropertyKeys
  ) {
    const encryptedValue = await encryptWithPassword(value, password)
    return await setToEncryptedStorage(storageKey, encryptedValue)
  }

  async retrieveAndDecryptFromDevice(
    password: string,
    storageKey: StoragePropertyKeys
  ): Promise<string | undefined> {
    const value = await getFromEncryptedStorage(storageKey)
    if (!value) throw new Error(`Missing value to decrypt from encrypted storage`)
    return await decryptWithPassword(value, password)
  }

  static txInputsToAssets(txInputs: TxInput[]): Assets[] {
    return txInputs.map((txIn) => txIn.value.assets)
  }

  static assetsToUnits(assetsArray: Assets[]) {
    let units = []

    for (let assets of assetsArray) {
      let tokens = assets.dump()

      for (let parentKey in tokens) {
        for (let childKey in tokens[parentKey]) {
          const { policyId, name, label } = fromAssetUnit(parentKey + childKey)

          const newObj = {
            policyId,
            name,
            label,
            count: tokens[parentKey][childKey],
          }
          units.push(newObj)
        }
      }
    }

    return units.flat()
  }

  async signTransaction() {}

  async getTransactions() {}

  async getUtxosAtAddress(addr: string) {
    let error
    let data
    try {
      data = await this.BlockfrostAPI.getUtxos(new Address(addr))
    } catch (e) {
      error = e
    }

    return { error, data }
  }

  static async getNetworkParams() {
    try {
      const response = await fetch(
        `https://d1t0d7c2nekuk0.cloudfront.net/${process.env.CARDANO_NETWORK}.json`
      )
      const data = await response.json()
      return data
    } catch (e) {
      throw e
    }
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
}
