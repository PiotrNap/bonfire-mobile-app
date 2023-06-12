global.Buffer = require("safe-buffer").Buffer

if (typeof BigInt === "undefined") {
  const BigInt = require("big-integer")
  global.BigInt = BigInt
}
