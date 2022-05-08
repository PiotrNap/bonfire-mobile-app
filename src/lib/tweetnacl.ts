import nacl from "@pnap/react-native-tweetnacl"
import base64 from "base64-js"
import { getFromEncryptedStorage } from "./encryptedStorage"

interface KeyPair {
  publicKey: Uint8Array
  secretKey: Uint8Array
}

export const generateKeyPair = async (): Promise<KeyPair | void> => {
  try {
    const keypair = nacl.sign.keyPair()
    return keypair
  } catch (e) {
    console.error(e)
  }
}

export const signChallenge = async (
  challenge: Uint8Array,
  secretKey?: Uint8Array
): Promise<string | void> => {
  var _secretKey, signedChallenge

  _secretKey = secretKey || (await getFromEncryptedStorage("privKey"))

  if (!_secretKey || !challenge) return
  _secretKey = base64.toByteArray(_secretKey)

  // TODO: use a byte-array when the device API's support that type, and fill the buffer with zeros immediately after use.
  try {
    console.log("before signing :", challenge, _secretKey)
    signedChallenge = nacl.sign.detached(challenge, _secretKey)

    console.log("our signature ", base64.fromByteArray(signedChallenge))

    _secretKey = null
    secretKey = new Uint8Array([])
    challenge = new Uint8Array([])

    return base64.fromByteArray(signedChallenge)
  } catch (e) {
    _secretKey = null
    console.error(e)
  }
}
