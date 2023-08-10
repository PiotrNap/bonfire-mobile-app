import { mnemonicToEntropy } from "bip39"
import {
  BaseAddress,
  Bip32PrivateKey,
  Bip32PublicKey,
  Optional,
  StakeCredential,
} from "@emurgo/csl-mobile-bridge"
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
import { WalletKeys } from "./types"

export class Wallet {
  chainConfig: AnyObject

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
  ): Promise<Optional<string>> {
    const chainKey = await accountPubKey.derive(ROLE_TYPE.EXTERNAL_CHAIN)
    if (!chainKey)
      throw new Error(`Unable to derive chain-key from account pub-key`)

    const stakingKey = await accountPubKey.derive(ROLE_TYPE.STAKING_KEY)
    if (!stakingKey)
      throw new Error(`Unable to derive staking-key from account pub-key`)

    const chainBip32PubKey = await chainKey.derive(index)
    if (!chainBip32PubKey)
      throw new Error(
        `Unable to derive chain's bip32 pub-key from account chain-key`
      )

    const chainKeyAddr = await chainBip32PubKey.to_raw_key()

    const stakingBig32PubKey = await stakingKey.derive(
      CONFIG_NUMBERS.STAKING_ACCOUNT_INDEX
    )
    if (!stakingBig32PubKey)
      throw new Error(
        `Unable to derive staking's bip32 pub-key from account chain-key`
      )

    const stakingKeyAddr = await stakingBig32PubKey.to_raw_key()

    const baseAddress = await BaseAddress.new(
      this.chainConfig.CHAIN_NETWORK_ID,
      await StakeCredential.from_keyhash(await chainKeyAddr.hash()),
      await StakeCredential.from_keyhash(await stakingKeyAddr.hash())
    )
    const rawAddress = await baseAddress.to_address()
    const bech32Address = rawAddress.to_bech32("")
    return bech32Address
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
  ): Promise<string | undefined> {
    const value = await getFromEncryptedStorage(storageKey)
    if (!value)
      throw new Error(`Missing value to decrypt from encrypted storage`)
    return await decryptWithPassword(value, password)
  }

  async signTransaction() {}

  async getTransactions() {}
}
