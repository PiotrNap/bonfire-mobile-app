import { generateMnemonic as bip39, mnemonicToEntropy } from "bip39"
import { randomBytes } from "react-native-randombytes"
import {
  BaseAddress,
  Bip32PrivateKey,
  Bip32PublicKey,
  StakeCredential,
} from "@emurgo/react-native-haskell-shelley"
import { setToEncryptedStorage } from "../encryptedStorage"
import { encryptWithPassword } from "../helpers"
import {
  ROLE_TYPE,
  CONFIG_NUMBERS,
  HASKELL_SHELLEY,
  HASKELL_SHELLEY_TESTNET,
} from "./config"
import { CARDANO_NETWORK } from "@env"
import { AnyObject } from "yup/lib/types"

export const generateMnemonic = bip39(128, randomBytes)

export class Wallet {
  chainConfig: AnyObject

  constructor() {
    this.chainConfig =
      CARDANO_NETWORK === "mainnet" ? HASKELL_SHELLEY : HASKELL_SHELLEY_TESTNET
  }

  async init(mnemonic: string, password: string): Promise<any> {
    try {
      const rootKeyPtr: Bip32PrivateKey = await this.createRootKey(mnemonic)
      const rootKey: string = Buffer.from(
        await rootKeyPtr.as_bytes()
      ).toString()

      const encryptedRootKey = await encryptWithPassword(rootKey, password)
      await setToEncryptedStorage("wallet-root-key", encryptedRootKey)

      // create extended priv and pub keys
      const accountKey = await (
        await (
          await rootKeyPtr.derive(CONFIG_NUMBERS.WALLET_TYPE_PURPOSE.CIP1852)
        ).derive(CONFIG_NUMBERS.COIN_TYPES.CARDANO)
      ).derive(
        CONFIG_NUMBERS.ACCOUNT_INDEX + CONFIG_NUMBERS.HARD_DERIVATION_START
      )

      const encryptedAccountKey = await encryptWithPassword(
        Buffer.from(await accountKey.as_bytes()).toString(),
        password
      )
      await setToEncryptedStorage("account-key", encryptedAccountKey)

      const accountPubKey = await accountKey.to_public()
      const encryptedAccountPubKey = encryptWithPassword(
        Buffer.from(await accountPubKey.as_bytes()).toString("hex"),
        password
      )
      await setToEncryptedStorage("account-pub-key", encryptedAccountPubKey)

      // for first release use first index only
      const baseAddress = await this.generateBaseAddress(accountPubKey, 0)

      return baseAddress
    } catch (e) {
      console.error(e)
      throw e
    }
  }

  async generateBaseAddress(
    accountPubKey: Bip32PublicKey,
    index: number
  ): Promise<string> {
    const chainKey = await accountPubKey.derive(ROLE_TYPE.EXTERNAL_CHAIN)
    const stakingKey = await (
      await (
        await accountPubKey.derive(ROLE_TYPE.STAKING_KEY)
      ).derive(CONFIG_NUMBERS.STAKING_ACCOUNT_INDEX)
    ).to_raw_key()

    const chainKeyAddr = await (await chainKey.derive(index)).to_raw_key()

    const addr = await BaseAddress.new(
      this.chainConfig.CHAIN_NETWORK_ID,
      await StakeCredential.from_keyhash(await chainKeyAddr.hash()),
      await StakeCredential.from_keyhash(await stakingKey.hash())
    )
    return await (await addr.to_address()).to_bech32()
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

  async createBaseAddressFromKey() {}

  async signTransaction() {}

  async getTransactions() {}
}
