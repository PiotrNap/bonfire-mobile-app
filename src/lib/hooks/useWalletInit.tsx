import { Wallet } from "lib/wallet"
import * as R from "react"

type Mnemonics = { [index: string]: string }

const testSeed = {
  0: "army",
  1: "screen",
  2: "aisle",
  3: "cover",
  4: "guide",
  5: "toast",
  6: "tooth",
  7: "enough",
  8: "orange",
  9: "derive",
  10: "syrup",
  11: "castle",
}

export const useWalletInit = () => {
  const [mnemonics, setMnemonics] = R.useState<Mnemonics | null>(testSeed)
  const [error, setError] = R.useState<string>("")

  const init = async () => {
    if (!mnemonics) return

    try {
      const wallet = await new Wallet().init(Object.values(mnemonics).join(" "))
      return wallet
    } catch (e) {
      setError(`Error: ${e.message}`)
      throw e
    }
  }

  return { error, mnemonics, setMnemonics, init }
}
