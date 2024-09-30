import { escrow_contract } from "./dist"
import { Program } from "@helios-lang/compat"
import { ContractContextBuilder } from "@helios-lang/contract-utils"
import type { NetworkId } from "lib/wallet/types"

export const createContractContext = (networkId: NetworkId) => {
  return ContractContextBuilder.new()
    .with(escrow_contract)
    .build({
      isMainnet: networkId === "Mainnet",
    })
}

export const escrowProgram = new Program(escrow_contract.$sourceCode).compile(true)
export const escrowValidatorHash = Buffer.from(escrowProgram.hash()).toString("hex")
