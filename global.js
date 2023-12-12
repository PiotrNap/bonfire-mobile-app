import { TextEncoder, TextDecoder } from "text-decoding"
import { init } from "@emurgo/cross-csl-mobile"

global.Buffer = require("safe-buffer").Buffer
global.TextEncoder = TextEncoder
global.TextDecoder = TextDecoder

// because pbkdf2 package expects this prop...
global.process.version = ""

export const CardanoMobile = init("global")

if (typeof BigInt === "undefined") {
  const BigInt = require("big-integer")
  global.BigInt = BigInt
}
