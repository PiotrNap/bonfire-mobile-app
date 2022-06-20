import { ANDROID_API_URL, IOS_API_URL } from "@env"
import { isIOS } from "lib/helpers"

export const CONFIG_NUMBERS = {
  HARD_DERIVATION_START: 2147483648,
  WALLET_TYPE_PURPOSE: {
    BIP44: 2147483692, // HARD_DERIVATION_START + 44;
    CIP1852: 2147485500, // HARD_DERIVATION_START + 1852;
  },
  COIN_TYPES: {
    CARDANO: 2147485463, // HARD_DERIVATION_START + 1815;
  },
  ACCOUNT_INDEX: 0,
  STAKING_ACCOUNT_INDEX: 0,
}

export const NETWORK_REGISTRY = {
  BYRON_MAINNET: 0,
  HASKELL_SHELLEY: 1,
  JORMUNGANDR: 100,
  // ERGO: 200,
  HASKELL_SHELLEY_TESTNET: 300,
  UNDEFINED: -1,
}

export const GENERAL_CONFIG = {
  BACKEND: {
    API_ROOT: isIOS ? IOS_API_URL : ANDROID_API_URL,
  },
  ENABLED: true,
  PER_EPOCH_PERCENTAGE_REWARD: 69344,
  COIN_TYPE: CONFIG_NUMBERS.COIN_TYPES.CARDANO,
  LINEAR_FEE: {
    COEFFICIENT: "44",
    CONSTANT: "155381",
  },
  MINIMUM_UTXO_VAL: "1000000",
  POOL_DEPOSIT: "500000000",
  KEY_DEPOSIT: "2000000",
}

export const HASKELL_SHELLEY_TESTNET = {
  PROVIDER_ID: NETWORK_REGISTRY.HASKELL_SHELLEY_TESTNET,
  NETWORK_ID: NETWORK_REGISTRY.HASKELL_SHELLEY_TESTNET,
  CHAIN_NETWORK_ID: 0,
  IS_MAINNET: false,
  EXPLORER_URL_FOR_ADDRESS: (address: string) =>
    `https://explorer.cardano-testnet.iohkdev.io/address?address=${address}`,
  EXPLORER_URL_FOR_TX: (tx: string) =>
    `https://explorer.cardano-testnet.iohkdev.io/tx/${tx}`,
  BASE_CONFIG: [
    {
      // shelley-era
      START_AT: 74,
      SLOTS_PER_EPOCH: 432000,
      SLOT_DURATION: 1,
    },
  ],
  ...GENERAL_CONFIG,
}

export const HASKELL_SHELLEY = {
  PROVIDER_ID: NETWORK_REGISTRY.HASKELL_SHELLEY,
  NETWORK_ID: NETWORK_REGISTRY.HASKELL_SHELLEY,
  MARKETING_NAME: "Cardano Mainnet",
  CHAIN_NETWORK_ID: 1,
  IS_MAINNET: true,
  EXPLORER_URL_FOR_ADDRESS: (address: string) =>
    `https://explorer.cardano.org/en/address?address=${address}`,
  EXPLORER_URL_FOR_TX: (tx: string) => `https://explorer.cardano.org/tx/${tx}`,
  BASE_CONFIG: [
    {
      // shelley-era
      START_AT: 208,
      SLOTS_PER_EPOCH: 432000,
      SLOT_DURATION: 1,
    },
  ],
  ...GENERAL_CONFIG,
}

export const ROLE_TYPE = {
  EXTERNAL_CHAIN: 0,
  INTERNAL_CHAIN: 1,
  STAKING_KEY: 2,
}
