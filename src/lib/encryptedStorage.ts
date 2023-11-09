import ES from "react-native-encrypted-storage"

// Just to minimize chance of collision with other apps when
// encrypted storage is shared among other apps.
export const generateUniqueEncryptedStorageKey = (val: any) => `${val}_bonfire`

/**
 * IMPORTANT !!!
 *
 * Never store sensitive credentials without first encrypting it.
 * (checkout helper functions for password-based encryption/decryption)
 */

export const setToEncryptedStorage = async (
  key: StoragePropertyKeys,
  value: any
): Promise<void> => {
  try {
    if (typeof value !== "string") value = JSON.stringify(value)

    await ES.setItem(generateUniqueEncryptedStorageKey(key), value)
  } catch (e) {
    console.error(e)
    throw new Error(e)
  }
}

export const removeFromEncryptedStorage = async (key: StoragePropertyKeys) => {
  try {
    await ES.removeItem(generateUniqueEncryptedStorageKey(key))
  } catch (e) {
    throw new Error(e)
  }
}

export const isAvailableEncryptedStorage = async (): Promise<boolean> => {
  try {
    await setToEncryptedStorage("test", "test")
    await removeFromEncryptedStorage("test")
    return true
  } catch {
    return false
  }
}

export const getFromEncryptedStorage = async (key: StoragePropertyKeys): Promise<any> => {
  return await ES.getItem(generateUniqueEncryptedStorageKey(key))
}

export const clearEncryptedStorage = async (): Promise<void> => {
  try {
    for (const property of STORAGE_PROPERTY_KEYS) {
      await ES.removeItem(generateUniqueEncryptedStorageKey(property))
    }
  } catch (e) {
    throw e
  }
}

export const STORAGE_PROPERTY_KEYS: StoragePropertyKeys[] = [
  "user-settings",
  "auth-credentials",
  "device-pubKey",
  "device-privKey",
  "device-id",
  "messaging-token",
  "time-zone",
  "test",
  "root-key",
  "wallet-name",
  "base-address",
  "account-#0-key",
  "account-#0-pub-key",
  "mnemonic",
]
export type StoragePropertyKeys =
  | "user-settings"
  | "auth-credentials"
  | "device-pubKey"
  | "device-privKey"
  | "device-id"
  | "messaging-token"
  | "time-zone"
  | "test"
  | "root-key"
  | "wallet-name"
  | "base-address"
  | "account-#0-key" // key from 1852/1815/0/0/0
  | "account-#0-pub-key"
  | "mnemonic"
