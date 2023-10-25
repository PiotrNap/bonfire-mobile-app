import { TextEncoder, TextDecoder } from "text-decoding"

global.Buffer = require("safe-buffer").Buffer
global.TextEncoder = TextEncoder
global.TextDecoder = TextDecoder

if (typeof BigInt === "undefined") {
  const BigInt = require("big-integer")
  global.BigInt = BigInt
}
