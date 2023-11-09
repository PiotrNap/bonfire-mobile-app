import AsyncStorage from "@react-native-async-storage/async-storage"
import { decryptWithPassword, encryptWithPassword } from "lib/helpers"
import { AnyObject } from "yup/lib/types"
import {
  getFromEncryptedStorage,
  removeFromEncryptedStorage,
  setToEncryptedStorage,
} from "lib/encryptedStorage"
import Keychain from "react-native-keychain"

export type AuthTypes = "password" | "device"

const BASE_SECURITY_OPTIONS = {
  accessControl: Keychain.ACCESS_CONTROL.BIOMETRY_ANY_OR_DEVICE_PASSCODE,
  accessible: Keychain.ACCESSIBLE.WHEN_UNLOCKED_THIS_DEVICE_ONLY,
  // ..also include service name for each credential
  securityLelel: Keychain.SECURITY_LEVEL.SECURE_HARDWARE, // Android specific
}

export async function addKeysToStorage(
  canStoreMnemonicOnDevice: boolean,
  deviceAuthAccepted: boolean,
  userPassword: string,
  rootKeyHex: string,
  baseAddress: string,
  baseAddressKey: string,
  mnemonic?: AnyObject
): Promise<void> {
  try {
    // ! No need for encrypting keys before storing them with react-native-keychain,
    // the device will do it automatically

    await AsyncStorage.setItem("account-#0-baseAddress", baseAddress) // unencrypted storage

    // this one is a must-have key storage method, in case nothing else is accepted/available
    const passwordEncryptedRootKey = await encryptWithPassword(rootKeyHex, userPassword)
    const passwordEncryptedBaseAddressKey = await encryptWithPassword(
      baseAddressKey,
      userPassword
    )

    await setToEncryptedStorage("root-key", passwordEncryptedRootKey)
    await setToEncryptedStorage("account-#0-key", passwordEncryptedBaseAddressKey)

    if (deviceAuthAccepted) {
      await Keychain.setGenericPassword("root-key", rootKeyHex, {
        ...BASE_SECURITY_OPTIONS,
        service: "@Bonfire:root-key",
      })
      await Keychain.setGenericPassword("account-#0-key", baseAddressKey, {
        ...BASE_SECURITY_OPTIONS,
        service: "@Bonfire:account-#0-key",
      })
    }

    if (mnemonic && canStoreMnemonicOnDevice) {
      const mnemonicPhrase = Object.values(mnemonic).join(" ")
      const passwordEncryptedMnemonic = await encryptWithPassword(
        mnemonicPhrase,
        userPassword
      )

      await setToEncryptedStorage("mnemonic", passwordEncryptedMnemonic)

      if (deviceAuthAccepted)
        await Keychain.setGenericPassword("mnemonic", mnemonicPhrase, {
          ...BASE_SECURITY_OPTIONS,
          service: "@Bonfire:mnemonic",
        })
    }
  } catch (e) {
    throw e
  } finally {
    userPassword = ""
    rootKeyHex = ""
    baseAddressKey = ""
    mnemonic = undefined
  }
}

// deletes every secret key stored on this device
export async function deleteKeysFromStorage(accountIndexes = [0]): Promise<void> {
  try {
    await removeFromEncryptedStorage("mnemonic")
    await Keychain.resetGenericPassword({ service: "@Bonfire:mnemonic" })

    await removeFromEncryptedStorage("root-key")
    await Keychain.resetGenericPassword({ service: "@Bonfire:root-key" })

    for (let accountIdx of accountIndexes) {
      //@ts-ignore because of dynamic `key` string value
      await removeFromEncryptedStorage(`account-#${accountIdx}-key`)
      await Keychain.resetGenericPassword({
        service: `@Bonfire:account-#${accountIdx}-key`,
      })
    }
    await AsyncStorage.removeItem("account-#0-baseAddress")

    console.log("All done. Storage keys deleted")
  } catch (e) {
    throw e
  }
}

// Function to retrieve mnemonic (from keystore or encrypted-storage)
export async function retrieveMnemonicPhraseFromStorage(
  authType: AuthTypes,
  password?: string
): Promise<string | void> {
  try {
    if (authType === "device") {
      const userCredentials = await Keychain.getGenericPassword({
        service: "@Bonfire:mnemonic",
      })
      if (!userCredentials || !userCredentials.password)
        throw new Error(
          "Missing mnemonic credential. Do you have a passcode or biometry set up?"
        )

      return password // password is the actual mnemonic
    } else if (authType === "password" && password) {
      console.log("hello ")
      const cipher = await getFromEncryptedStorage("mnemonic")
      if (!cipher)
        throw new Error("Missing mnemonic credential. Have you set up a password?")
      console.log("hello again")
      const decryptedMnemonic = await decryptWithPassword(cipher, password)
      return decryptedMnemonic
    }
  } catch (e) {
    throw new Error("Something went wrong. Maybe try a different method?")
  } finally {
    password = ""
  }
}

// Function to retrieve root Key (from keystore or encrypted-storage)
export async function retrieveRootKeyFromStorage(
  authType: AuthTypes,
  password?: string
): Promise<string | void> {
  try {
    if (authType === "device") {
      const userCredentials = await Keychain.getGenericPassword({
        service: "@Bonfire:root-key",
      })
      if (!userCredentials || !userCredentials.password)
        throw new Error(
          "Missing root key credential. Do you have a passcode or biometry set up?"
        )

      return password // password is the actual root key
    } else if (authType === "password" && password) {
      const cipher = await getFromEncryptedStorage("root-key")
      if (!cipher)
        throw new Error("Missing root key credential. Have you set up a password?")
      const decryptedMnemonic = await decryptWithPassword(cipher, password)
      return decryptedMnemonic
    }
  } catch (e) {
    throw new Error("Something went wrong. Maybe try a different method?")
  } finally {
    password = ""
  }
}

// Function to retrieve account priv-key (from keystore or encrypted-storage)
export async function retrieveAccountKeyFromStorage(
  authType: AuthTypes,
  password?: string,
  accountIndex: number = 0
): Promise<string | void> {
  try {
    if (authType === "device") {
      const userCredentials = await Keychain.getGenericPassword({
        service: `@Bonfire:account-#${accountIndex}-key`,
      })
      if (!userCredentials || !userCredentials.password)
        throw new Error(
          "Missing account key credential. Do you have a passcode or biometry set up?"
        )

      return password // password is the actual account key
    } else if (authType === "password" && password) {
      //@ts-ignore because of `key` string value being dynamic
      const cipher = await getFromEncryptedStorage(`account-#${accountIndex}-key`)
      if (!cipher)
        throw new Error("Missing account key credential. Have you set up a password?")
      const decryptedMnemonic = await decryptWithPassword(cipher, password)
      return decryptedMnemonic
    }
  } catch (e) {
    throw new Error("Something went wrong. Maybe try a different method?")
  } finally {
    password = ""
  }
}
