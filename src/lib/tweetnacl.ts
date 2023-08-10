import nacl from "@pnap/react-native-tweetnacl"
import base64 from "base64-js"
import { getFromEncryptedStorage } from "./encryptedStorage"

interface KeyPair {
  publicKey: Uint8Array
  secretKey: Uint8Array
}

export const generateKeyPair = (): KeyPair => {
  return nacl.sign.keyPair()
}

export const signChallenge = async (
  challenge: Uint8Array,
  secretKey?: Uint8Array
): Promise<string | void> => {
  var _secretKey, signedChallenge

  _secretKey = secretKey || (await getFromEncryptedStorage("privKey"))

  if (!_secretKey || !challenge) return
  _secretKey = Buffer.from(_secretKey, "base64")

  // TODO: use a byte-array when the device API's support that type,
  // and fill the buffer with zeros immediately after use.
  try {
    signedChallenge = nacl.sign.detached(challenge, _secretKey)

    _secretKey = null
    secretKey = new Uint8Array([])
    challenge = new Uint8Array([])

    return base64.fromByteArray(signedChallenge)
  } catch (e) {
    _secretKey = null
    console.error(e)
  }
}
