import nacl from "@pnap/react-native-tweetnacl"
import base64 from "base64-js"

interface KeyPair {
  publicKey: Uint8Array
  secretKey: Uint8Array
}

export const generateKeyPair = (): KeyPair => {
  return nacl.sign.keyPair()
}

export const signChallenge = async (
  challenge: Uint8Array,
  secretKey: Uint8Array
): Promise<string | void> => {
  var signedChallenge

  if (!challenge || !secretKey)
    throw new Error(
      `Missing challenge or secret key. Unable to sign a challenge`
    )
  try {
    signedChallenge = nacl.sign.detached(challenge, secretKey)
    return base64.fromByteArray(signedChallenge)
  } catch (e) {
    console.error(e)
  } finally {
    secretKey = new Uint8Array([])
    challenge = new Uint8Array([])
  }
}
