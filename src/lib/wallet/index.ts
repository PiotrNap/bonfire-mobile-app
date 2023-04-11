import { generateMnemonic as bip39, mnemonicToEntropy } from "bip39"
import { randomBytes } from "react-native-randombytes"
import {
  BaseAddress,
  Bip32PrivateKey,
  Bip32PublicKey,
  StakeCredential,
} from "@emurgo/react-native-haskell-shelley"
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
} from "./config"
import { CARDANO_NETWORK } from "@env"
import { AnyObject } from "yup/lib/types"

export const generateMnemonic = () => bip39(128, randomBytes)

export class Wallet {
  chainConfig: AnyObject

  constructor() {
    this.chainConfig =
      CARDANO_NETWORK === "mainnet" ? HASKELL_SHELLEY : HASKELL_SHELLEY_TESTNET
  }

  /**
   * Creates root key and account keys with base address
   */
  async init(mnemonic: string): Promise<any> {
    try {
      const rootKeyPtr: Bip32PrivateKey = await this.createRootKey(mnemonic)
      const rootKey: string = Buffer.from(await rootKeyPtr.as_bytes()).toString(
        "hex"
      )

      const accountKeyPtr = await (
        await (
          await rootKeyPtr.derive(CONFIG_NUMBERS.WALLET_TYPE_PURPOSE.CIP1852)
        ).derive(CONFIG_NUMBERS.COIN_TYPES.CARDANO)
      ).derive(
        CONFIG_NUMBERS.ACCOUNT_INDEX + CONFIG_NUMBERS.HARD_DERIVATION_START
      )
      const accountKey = Buffer.from(await accountKeyPtr.as_bytes()).toString(
        "hex"
      )

      const accountPubKeyPtr = await accountKeyPtr.to_public()
      const accountPubKey = Buffer.from(
        await accountPubKeyPtr.as_bytes()
      ).toString("hex")

      // for first release use first index only
      const baseAddress = await this.generateBaseAddress(accountPubKeyPtr, 0)

      return { baseAddress, rootKey, accountKey, accountPubKey }
    } catch (e) {
      console.error(e)
      throw e
    }
  }

  async generateBaseAddress(
    accountPubKey: Bip32PublicKey,
    index: number
  ): Promise<string | undefined> {
    const chainKey = await accountPubKey.derive(ROLE_TYPE.EXTERNAL_CHAIN)
    const stakingKey = await accountPubKey.derive(ROLE_TYPE.STAKING_KEY)
    if (!chainKey || !stakingKey) return

    const derivedIdx = await chainKey.derive(index)
    const chainKeyAddr = await derivedIdx?.to_raw_key()
    const stakingDerivedIdx = await stakingKey.derive(
      CONFIG_NUMBERS.STAKING_ACCOUNT_INDEX
    )
    const stakingKeyAddr = await stakingDerivedIdx?.to_raw_key()

    if (!chainKeyAddr || !stakingKeyAddr) return

    const chainKeyAddrHash = await chainKeyAddr.hash()
    const stakingKeyAddrHash = await stakingKeyAddr.hash()

    const addr = await BaseAddress.new(
      this.chainConfig.CHAIN_NETWORK_ID,
      await StakeCredential.from_keyhash(chainKeyAddrHash),
      await StakeCredential.from_keyhash(stakingKeyAddrHash)
    )
    return await (await addr.to_address()).to_bech32(undefined)
  }

  async createRootKey(mnemonic: string): Promise<Bip32PrivateKey> {
    const entropy = mnemonicToEntropy(mnemonic)
    const password = Buffer.from("")
    const rootKey = await Bip32PrivateKey.from_bip39_entropy(
      Buffer.from(entropy, "hex"),
      password
    )

    return rootKey
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
  ): Promise<undefined | string> {
    const value = await getFromEncryptedStorage(storageKey)
    return await decryptWithPassword(value, password)
  }

  async signTransaction() {}

  async getTransactions() {}
}
