import {
  Address,
  Assets,
  hexToBytes,
  MintingPolicyHash,
  NetworkParams,
  TxInput,
  Value,
} from "@hyperionbt/helios"
import { crc8 } from "crc"
import { CARDANO_NETWORK } from "@env"
import { mainnet } from "../../on_chain/configs/mainnet.js"
import { preprod } from "../../on_chain/configs/preprod.js"
import { AssetUnit, PaymentTokens, WalletAssets } from "./types"
import { HourlyRate } from "common/interfaces/bookingInterface.js"

export const networkParams: NetworkParams = new NetworkParams(
  CARDANO_NETWORK === "mainnet" ? mainnet : preprod
)

function checksum(num) {
  return crc8(Buffer.from(num, "hex")).toString(16).padStart(2, "0")
}

export function toLabel(num: number) {
  if (num < 0 || num > 65535) {
    throw new Error(`Label ${num} out of range: min label 1 - max label 65535.`)
  }
  const numHex = numberToHex(num).padStart(4, "0")

  return "0" + numHex + checksum(numHex) + "0"
}

export function fromLabel(label) {
  if (label?.length !== 8 || !(label[0] === "0" && label[7] === "0")) {
    return null
  }
  const numHex = label.slice(1, 5)
  const num = parseInt(numHex, 16)
  const check = label.slice(5, 7)
  return check === checksum(numHex) ? num : null
}

export function toAssetUnit(policyId, name, label) {
  const hexLabel = Number.isInteger(label) ? toLabel(label) : label
  const n = name ? name : ""
  if ((n + hexLabel).length > 64) {
    throw new Error("Asset name size exceeds 32 bytes.")
  }
  if (policyId.length !== 56) {
    throw new Error(`Policy id invalid: ${policyId}.`)
  }
  return policyId + hexLabel + n
}

export function fromAssetUnit(unit) {
  const policyId = unit.slice(0, 56)
  const label = fromLabel(unit.slice(56, 64))
  const name = (() => {
    const hexName = Number.isInteger(label) ? unit.slice(64) : unit.slice(56)
    return unit.length === 56 ? "" : hexName || null
  })()
  return { policyId, name, label }
}

export function lovelaceToAda(lovelace: bigint) {
  return Number(lovelace) / 1_000_000
}

export function lovelaceValueOfInputs(inputs: TxInput[]): bigint {
  return inputs.reduce((prev, curr) => prev + curr.value.lovelace, 0n)
}

export function filterLovelaceOnlyInputs(inputs: TxInput[]): TxInput[] {
  return inputs.filter((txIn) => !txIn.value.assets.mintingPolicies.length)
}

export function txInputsToAssets(txInputs: TxInput[]): Assets[] {
  return txInputs.map((txIn) => txIn.value.assets)
}

// converts Helios Assets to Array of [Unit, UnitDetails]
export function assetsToUnitsArray(assetsArray: Assets[]): [string, AssetUnit][] {
  let units: [string, AssetUnit][] = []
  for (let assets of assetsArray) {
    let tokens = assets.dump()

    for (let parentKey in tokens) {
      for (let childKey in tokens[parentKey]) {
        const unit = parentKey + childKey
        const existingUnit = units.find((u) => u[0] === unit)
        const { policyId, name, label } = fromAssetUnit(unit)
        const newObj = {
          policyId,
          name,
          label: label ? toLabel(label) : "",
          count: tokens[parentKey][childKey],
        }

        if (existingUnit) {
          newObj.count = Number(newObj.count) + Number(existingUnit[1].count)
          units.push([unit, newObj])
          continue
        }

        units.push([unit, newObj])
      }
    }
  }
  return units
}

// converts an iterable `units<Unit, UnitDetails>` to Assets which can be used to construct Helios Value
export function unitsToAssets(
  units: HourlyRate | WalletAssets | AssetUnit[]
): Assets | undefined {
  if (!units) return
  let tokens: any[] = []

  for (let unitDetails of units.values()) {
    let mph = new MintingPolicyHash(unitDetails.policyId)
    let token: [number[], bigint][] = [
      [hexToBytes(unitDetails.label || "" + unitDetails.name), BigInt(unitDetails.count)],
    ]
    tokens.push([mph, token])
  }

  return new Assets(tokens)
}

export function schemaToPaymentTokens(schema: string): PaymentTokens {
  schema = JSON.parse(schema)
  const paymentTokenAssetsArray: any[] = []
  let paymentLovelace = 0

  schema.map.forEach((entry) => {
    const assetId = entry.k.bytes
    if (assetId === "") {
      // Extract lovelace value
      paymentLovelace = entry.v.map[0].v.int
    } else {
      // Extract token information
      const tokensArray = entry.v.map.map((tokenEntry) => {
        const tokenName = tokenEntry.k.bytes
        const tokenAmt = BigInt(tokenEntry.v.int)
        return [tokenName, tokenAmt]
      })
      paymentTokenAssetsArray.push([assetId, tokensArray])
    }
  })

  const paymentTokenAssets = new Assets(paymentTokenAssetsArray)
  return { lovelace: paymentLovelace, assets: paymentTokenAssets }
}

export function assetsUnitsToValue(assetsUnits: AssetUnit[]): Value {
  let _assetsUnits = [...assetsUnits]
  let assetsUnitsLovelace = Number(_assetsUnits[0].count)

  // convert to lovelace (good for now solution)
  if (assetsUnitsLovelace < 1_000_000) assetsUnitsLovelace *= 1_000_000
  _assetsUnits.shift()
  const assetsUnitsValue = new Value(
    BigInt(assetsUnitsLovelace),
    unitsToAssets(
      _assetsUnits.map((unit) => ({ ...unit, label: "", name: utf8ToHex(unit.name) }))
    )
  )
  return assetsUnitsValue
}

export function assetsUnitsToJSONSchema(assetsUnits: AssetUnit[]): string {
  return assetsUnitsToValue(assetsUnits).toSchemaJson()
}

export function numberToHex(n: number) {
  let hex = ""
  while (n) {
    hex = String.fromCharCode((n % 16) + (n % 16 < 10 ? 48 : 87)) + hex
    n = Math.floor(n / 16)
  }
  return hex
}

export function hexToUtf8(hexString: string) {
  return Buffer.from(hexString, "hex").toString("utf8")
}

export function utf8ToHex(string: string) {
  return Buffer.from(string).toString("hex")
}

export function ipfsToHttp(ipfsUrl: string | string[]) {
  if (Array.isArray(ipfsUrl)) ipfsUrl = ipfsUrl.join("")
  const ipfsGateway = process.env.IPFS_GATEWAY
  let ipfsHash = ""

  if (ipfsUrl?.startsWith("ipfs://"))
    ipfsHash = ipfsUrl.split("ipfs://")[1].split("ipfs/").slice(-1)[0]

  return ipfsGateway + ipfsHash
}

export function checkBech32Address(addr: string) {
  return Address.fromBech32(addr)?.toBech32()
}
