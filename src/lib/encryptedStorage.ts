import ES from "react-native-encrypted-storage"

// Just to minimize chance of collision with other apps when
// encrypted storage is shared among other apps.
const generateKey = (val: any) => `${val}_bonfire`

export const setToEncryptedStorage = async (
  key: StoragePropertyKeys,
  value: any
): Promise<void> => {
  try {
    if (typeof value !== "string") value = JSON.stringify(value)

    await ES.setItem(generateKey(key), value)
  } catch (e) {
    console.error(e)
    throw new Error(e)
  }
}

export const removeFromEncryptedStorage = async (key: StoragePropertyKeys) => {
  try {
    await ES.removeItem(generateKey(key))
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

export const getFromEncryptedStorage = async (
  key: StoragePropertyKeys
): Promise<any> => {
  return await ES.getItem(generateKey(key))
}

export const clearEncryptedStorage = async (): Promise<boolean> => {
  try {
    for (const property of storagePropertyKeys) {
      await ES.removeItem(generateKey(property))
    }
    return true
  } catch (e) {
    return false
  }
}

export const storagePropertyKeys: StoragePropertyKeys[] = [
  "user-settings",
  "auth-credentials",
  "device-pubKey",
  "device-privKey",
  "device-id",
  "messaging-token",
  "time-zone",
  "test",
  "wallet-root-key",
  "wallet-name",
  "wallet-base-address",
  "account-key",
  "account-pub-key",
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
  | "wallet-root-key"
  | "wallet-name"
  | "wallet-base-address"
  | "account-key"
  | "account-pub-key"
  | "mnemonic"
