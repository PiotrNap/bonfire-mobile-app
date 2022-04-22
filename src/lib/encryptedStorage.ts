import ES from "react-native-encrypted-storage"

// Just to minimize chance of collision with other apps when
// encrypted storage is shared among other apps.
const generateKey = (val: any) => `${val}_bonfire`

export const setToEncryptedStorage = async (
  key: StoragePropertyKeys,
  value: any
): Promise<void> => {
  try {
    value = JSON.stringify(value)

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
  try {
    const val = await ES.getItem(generateKey(key))

    if (val != null) {
      return JSON.parse(val)
    } else {
      return null
    }
  } catch (e) {
    throw new Error(e)
  }
}

export const clearEncryptedStorage = async (): Promise<boolean> => {
  try {
    await ES.clear()
    return true
  } catch (e) {
    return false
  }
}

export type StoragePropertyKeys =
  | "user-settings"
  | "auth-credentials"
  | "pubKey"
  | "privKey"
  | "test"
