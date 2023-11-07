import { Address, Assets, NetworkParams, TxInput } from "@hyperionbt/helios"
import { crc8 } from "crc"
import { CARDANO_NETWORK } from "@env"
import MainnetParams from "../../on_chain/configs/mainnet.json"
import PreprodParams from "../../on_chain/configs/preprod.json"

export const networkParams: NetworkParams = new NetworkParams(
  CARDANO_NETWORK === "mainnet" ? MainnetParams : PreprodParams
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
  const hexLabel = Number.isInteger(label) ? toLabel(label) : ""
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

export function lovelaceValueOfInputs(inputs: TxInput[]): BigInt {
  return inputs.reduce((prev, curr) => prev + curr.value.lovelace, 0n)
}

export function filterLovelaceOnlyInputs(inputs: TxInput[]): TxInput[] {
  return inputs.filter((txIn) => !txIn.value.assets.mintingPolicies.length)
}

export function txInputsToAssets(txInputs: TxInput[]): Assets[] {
  return txInputs.map((txIn) => txIn.value.assets)
}

export function assetsToUnitsMap(assetsArray: Assets[]) {
  let units = new Map()
  for (let assets of assetsArray) {
    let tokens = assets.dump()

    for (let parentKey in tokens) {
      for (let childKey in tokens[parentKey]) {
        const unit = parentKey + childKey
        const existingUnit = units.get(unit)
        const { policyId, name, label } = fromAssetUnit(unit)
        const newObj = {
          policyId,
          name,
          labelNum: label,
          count: tokens[parentKey][childKey],
        }

        if (existingUnit) {
          newObj.count = Number(newObj.count) + Number(existingUnit.count)
          units.set(unit, newObj)
          continue
        }

        units.set(unit, newObj)
      }
    }
  }
  return units
}

export function numberToHex(n: number) {
  let hex = ""
  while (n) {
    hex = String.fromCharCode((n % 16) + (n % 16 < 10 ? 48 : 87)) + hex
    n = Math.floor(n / 16)
  }
  return hex
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