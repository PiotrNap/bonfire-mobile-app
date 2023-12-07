import {
  Address,
  Assets,
  hexToBytes,
  MintingPolicyHash,
  NetworkParams,
  TxInput,
  TxOutput,
  Value,
} from "@hyperionbt/helios"
import { crc8 } from "crc"
import {
  CARDANO_NETWORK,
  MIN_LOVELACE_SERVICE_FEE,
  MIN_PERCENT_SERVICE_FEE,
  BETA_TESTER_MPH,
} from "@env"
import { mainnet } from "../../on_chain/configs/mainnet.js"
import { preprod } from "../../on_chain/configs/preprod.js"
import { AssetUnit, PaymentTokens, WalletAssets } from "./types"
import { HourlyRate } from "common/interfaces/bookingInterface.js"
import { getNetworkConfig } from "./"

export const COLLATERAL_LOVELACE = 5_000_000n
export const COLLATERAL_STORAGE_KEY = "collateral-utxoid"

export const networkParams: NetworkParams = new NetworkParams(
  CARDANO_NETWORK === "mainnet" ? mainnet : preprod
)

function checksum(num) {
  return crc8(Buffer.from(num, "hex")).toString(16).padStart(2, "0")
}

// used to cut long strings into shorter format (address ,txHash, ...)
export function cutStringInside(str: string | undefined) {
  if (!str) return
  return `${str.substring(0, 15)}...${str.substring(str.length - 15, str.length)}`
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
  units: HourlyRate | WalletAssets | AssetUnit[] | undefined
): Assets | undefined {
  if (!units) return
  let tokens: any[] = []

  for (let unitDetails of units.values()) {
    let mph = new MintingPolicyHash(unitDetails.policyId)
    let token: [number[], bigint][] = [
      [
        hexToBytes((unitDetails.label || "") + unitDetails.name),
        BigInt(unitDetails.count),
      ],
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

export function assetsUnitsToValue(
  lovelaceAsset: AssetUnit,
  nativeAssets: AssetUnit[]
): Value {
  let lovelaceCount = Number(lovelaceAsset.count)

  // convert to lovelace (good for now solution)
  // ...less than 1mil means its ADA
  if (lovelaceCount < 1_000_000) lovelaceCount *= 1_000_000
  const assetsUnitsValue = new Value(
    BigInt(lovelaceCount),
    unitsToAssets(nativeAssets.map((unit) => ({ ...unit, label: "", name: unit.name })))
  )
  return assetsUnitsValue
}

export function assetsUnitsToJSONSchema(
  lovelaceAsset: AssetUnit,
  nativeAssets: AssetUnit[]
): string {
  return assetsUnitsToValue(lovelaceAsset, nativeAssets).toSchemaJson()
}

export function calculateFeeForTreasury(txInput: TxInput): bigint {
  const totalLovelaceValue = lovelaceValueOfInputs([txInput])

  if (totalLovelaceValue / 100n < MIN_LOVELACE_SERVICE_FEE) {
    return BigInt(MIN_LOVELACE_SERVICE_FEE)
  } else {
    return BigInt(totalLovelaceValue) * BigInt(MIN_PERCENT_SERVICE_FEE / 100n)
  }
}

export function checkForBetaTesterToken(utxos: TxInput[]) {
  return !!utxos.find(
    (txInput) =>
      txInput.value.assets.getTokens(new MintingPolicyHash(BETA_TESTER_MPH)).length > 0
  )
}

export function checkForCollateralAndFeeUtxos(
  utxos: TxInput[],
  serviceFee: bigint,
  collateralUtxoId: string | null
): {
  collateralUtxo: TxInput | null
  feeUtxo: TxInput | null
  spareUtxos: TxInput[] | []
  hasEnoughFunds: boolean // whether users' wallet have enough funds to cover collateral and all tx fee
  missingLovelace: bigint
} {
  const networkParams = new NetworkParams(getNetworkConfig())
  let collateralUtxo = null,
    feeUtxo = null,
    spareUtxos: TxInput[] = [],
    totalMinUtxoLovelace: bigint = 0n,
    totalLovelace: bigint = 0n,
    totalNeccessaryLovelace = COLLATERAL_LOVELACE + serviceFee

  for (let utxo of utxos) {
    let v = utxo.value

    if (collateralUtxoId === `${utxo.outputId.txId}#${utxo.outputId.utxoIdx}`) {
      collateralUtxo = utxo
    }

    if (v.assets.isZero()) {
      // assuming the total on-chain unlocking-tx fee will be 0.75 ADA
      // @TODO make this a variable once we support withdrawing multiple payouts
      if (v.lovelace > serviceFee + 750_000n && !feeUtxo) {
        // pick the one with lowest lovelace
        feeUtxo = utxo
      } else if (
        v.lovelace >= COLLATERAL_LOVELACE &&
        (!collateralUtxo || collateralUtxo.value.lovelace > v.lovelace)
      ) {
        collateralUtxo = utxo
      } else {
        spareUtxos.push(utxo)
      }
    } else {
      let availableLovelace = v.lovelace - calculateUTXOMinLovelace(utxo, networkParams)

      if (availableLovelace > serviceFee + 500_000n && !feeUtxo) {
        feeUtxo = utxo
      } else {
        spareUtxos.push(utxo)
      }
    }

    totalLovelace += utxo.value.lovelace
    totalMinUtxoLovelace += utxo.output.calcMinLovelace(networkParams)
  }

  const hasEnoughFunds = totalNeccessaryLovelace < totalLovelace - totalMinUtxoLovelace
  const missingLovelace = totalNeccessaryLovelace - (totalLovelace - totalMinUtxoLovelace)
  // console.log("totalLovelace >", totalLovelace)
  // console.log("totalNeccessaryLovelace >", totalNeccessaryLovelace)
  // console.log("totalMinUtxoLovelace >", totalMinUtxoLovelace)
  // console.log("hasEnoughFunds >", hasEnoughFunds)
  // console.log("collateralUtxo.value.dump() >", collateralUtxo?.value.dump())

  return { collateralUtxo, feeUtxo, spareUtxos, hasEnoughFunds, missingLovelace }
}

export function calculateUTXOMinLovelace(
  utxo: TxInput | TxOutput,
  networkParams: NetworkParams
) {
  //@ts-ignore becuase of the getter being internal
  const lovelacePerByte = networkParams.lovelacePerUTXOByte

  let correctedSize = utxo.toCbor().length + 160 // 160 accounts for some database overhead?

  return BigInt(correctedSize) * BigInt(lovelacePerByte)
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
