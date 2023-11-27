import * as React from "react"

import { StackScreenProps } from "@react-navigation/stack"
import { EventCreationParamList } from "common/types/navigationTypes"
import { ErrorHandler } from "components/errors/errorHandler"
import { Calendar } from "containers/MyCalendar"
import { MyCalendarProvider } from "contexts/myCalendarContext"
import { ValueOf } from "react-native-gesture-handler/lib/typescript/typeUtils"
import { Wallet } from "lib/wallet"
import { randomBytes } from "react-native-randombytes"
import RB from "react-native-randombytes"
import EC from "../on_chain/EscrowContract.js"

import {
  Address,
  Assets,
  Bip32PrivateKey,
  BlockfrostV0,
  bytesToText,
  Crypto,
  Datum,
  NetworkParams,
  Program,
  RootPrivateKey,
  textToBytes,
  Tx,
  TxInput,
  TxOutput,
  Value,
} from "@hyperionbt/helios"
import { entropyToMnemonic } from "bip39"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { useFocusEffect } from "@react-navigation/native"

export interface HomeProps extends StackScreenProps<EventCreationParamList, "Home"> {}

export const HomeScreen = ({ navigation }: HomeProps) => {
  const navigateCb = (
    name: keyof EventCreationParamList,
    params: ValueOf<EventCreationParamList>
  ) => navigation.navigate(name, params)

  const run = async () => {
    // let bytes = textToBytes("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    // console.log(bytes.length)
    // let res = Crypto.sha2_512(bytes, "0")

    // const escrowProgram = Program.new(EC)
    // const escrowProgramCompiled = escrowProgram.compile(false)

    const network: any = process.env.CARDANO_NETWORK
    const apiKey: any = process.env.BLOCKFROST_KEY

    let blockFrost = new BlockfrostV0(network, apiKey || "")
    console.log(network, apiKey)
    // return

    try {
      // let mnemonic = {
      //   "0": "expand",
      //   "1": "good",
      //   "10": "spoon",
      //   "11": "tag",
      //   "2": "gun",
      //   "3": "morning",
      //   "4": "wall",
      //   "5": "assault",
      //   "6": "heart",
      //   "7": "punch",
      //   "8": "access",
      //   "9": "magic",
      // }
      let mnemonic =
        "axis chimney fox sun once sad learn honey cactus spoil romance bench antique protect message"
      let beneficiary = new Address(
        "addr_test1qqtd83qaffaczz26lruytuygqc2fx396h45t6x9lxpq7k6v39lqt9m00ckwzlhczj8w8fspaqrtgzhaxagtuxpun9xyqzz09rk"
      )
      let escrowScriptAddr = new Address(
        "addr_test1wqslj3jfjj6umv2c02xyshnktse4zfpzns5r9v9jv8l5cxczvk2vn"
      )
      let userID = "72e9680e-0cfb-4766-ae29-aaeceaa7b581"
      const start = performance.now() / 1000
      let walletKeys = new Wallet().init(mnemonic)
      const end = performance.now() / 1000
      console.log(end - start)

      console.log(walletKeys)
      return
      // let privKey = RootPrivateKey.fromPhrase(mnemonic.split(" "))
      // console.log("here? ", privKey.toPhrase())
      // let bytes = textToBytes("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")

      /**
       * PROFILING sha2_512
       */
      // function isOdd(n) {
      //   return Math.abs(n % 2) == 1
      // }
      // let bytesEven = new Uint8Array(192).fill(54)
      // let bytesOdd = new Uint8Array(192).fill(92)
      // let result = []
      // for (let i = 1; i <= 100; i++) {
      //   let bytes = isOdd(i) ? bytesOdd : bytesEven
      //   let [_, logs, iterationTime] = Crypto.sha2_512(Array.from(bytes))
      //   result.push({ executionTime: iterationTime, sha2_512: logs })
      // }
      // return console.log(JSON.stringify(result, null, 4))
      // ------------------------

      // let mnemonic: string | undefined = await Wallet.generateADAMnemonic()

      let validatedUnlockingTxn = await Wallet.validatePlutusScriptOffchain(
        // rawUnlockingTxn.toCbor(),
        // txInputs,
        // escrowProgramCompiled.toCbor(),
        userID
      )
      return

      let { baseAddress, accountKeyHex } = await new Wallet().init(mnemonic)
      let privKeyBuffArray2 = Array.from(Buffer.from(accountKeyHex, "hex"))
      //let privKey = new Bip32PrivateKey(privKeyBuffArray2).derive(0).derive(0)
      let benefactor = new Address(baseAddress)

      // each event booking will have the payment token info, so I don't have to call blockfrost here

      const paymentTokenName = textToBytes("PIGGY")
      const paymentTokenAmt = BigInt(5000)
      const paymentToken: [number[], bigint][] = [[paymentTokenName, paymentTokenAmt]]
      const paymentTokenAssets = new Assets([
        ["6574e6ba313af9f4bbcb1ffde334a9a13f7120190ea3c4aa6530a6ac", paymentToken],
      ])
      const paymentLovelace = 25_000_000
      const paymentTokens = new Value(paymentLovelace, paymentTokenAssets)
      const schema = paymentTokens.toSchemaJson()

      // return console.log(paymentTokens)
      let utxosAtAddr = await blockFrost.getUtxos(benefactor)
      let escrowUtxos = await blockFrost.getUtxos(escrowScriptAddr)

      const createdAt = Date.now()
      const cancelWindowStart = createdAt
      const cancelWindowEnd = createdAt + 1000 * 60 * 60 * 24 // 24 hours
      const releaseDate = cancelWindowEnd
      const escrowDatum = new escrowProgram.types.Datum(
        beneficiary?.pubKeyHash?.hex, // beneficiaryPkh
        benefactor?.pubKeyHash?.hex, // benefactorPkh
        BigInt(Math.floor(releaseDate)), // releaseDate
        20, // cancelFee %
        BigInt(Math.floor(cancelWindowStart)), // cancelWindowStart
        BigInt(Math.floor(cancelWindowEnd)), // cancelWindowEnd
        BigInt(Math.floor(createdAt)), // createdAt
        paymentTokens
      )
      const inlineDatum = Datum.inline(escrowDatum)
      const lockingTxOutput = new TxOutput(escrowScriptAddr, paymentTokens, inlineDatum)

      const lockingTx = new Tx().addInputs(utxosAtAddr).addOutput(lockingTxOutput)

      // let walletEmulator = new WalletEmulator(blockFrost, privKey)
      // console.log("1 ", JSON.stringify(utxosAtAddr[0].value.dump(), null, 4))
      // console.log("2 ", JSON.stringify(escrowUtxos[0].value.dump(), null, 4))
      // console.log(JSON.stringify(utxosAtAddr, null, 4))

      let params = new NetworkParams(await Wallet.getNetworkParams())
      let currSlot = BigInt(params.raw.latestTip.slot)

      // const escrowTxIds = escrowUtxos.map((utxo) => utxo.outputId)

      // const redeemer = new escrowProgram.types.Redeemer.Cancel(
      //   escrowTxIds[0].txId.hex,
      //   escrowTxIds[0].utxoIdx
      // )

      // // const outValue = new Value({ lovelace: 2_000_000 })
      // const txOutput = new TxOutput(benefactor, paymentTokens)

      // const rawUnlockingTxn = new Tx()
      //   .attachScript(escrowProgramCompiled)
      //   .addInputs(escrowUtxos, redeemer)
      //   .addInputs(utxosAtAddr)
      //   .addOutput(txOutput)
      //   .addOutput(new TxOutput(beneficiary, new Value({ lovelace: 1_500_000n })))
      //   .validFrom(currSlot)
      //   .validTo(currSlot + BigInt(10))

      // const before = rawUnlockingTxn.body.inputs.map((i) => i.dump())

      // const txInputs = rawUnlockingTxn.body.inputs.map((input) => input.toFullCbor())
      // const after = txInputs.map((txIn) => TxInput.fromCbor(txIn))[0]
      // const  = (rawUnlockingTxn.dump().body.inputs as TxInput)

      // console.log(JSON.stringify(rawUnlockingTxn.dump().body.inputs, null, 4))

      // const backEndTx = await Wallet.validatePlutusScriptOffchain(userID)
      // console.log("??? ... ", backEndTx)

      // let validatedUnlockingTxn = await Wallet.validatePlutusScriptOffchain(
      //   rawUnlockingTxn.toCbor(),
      //   txInputs,
      //   escrowProgramCompiled.toCbor(),
      //   userID
      // )

      // const unlockingTxn = Tx.fromCbor(validatedUnlockingTxn)

      await lockingTx.finalize(params, benefactor)
      // let signature = await walletEmulator.signTx(unlockingTxn)
      let signature = privKey.sign(lockingTx.bodyHash)
      lockingTx.addSignatures([signature])
      const res = await blockFrost.submitTx(lockingTx)

      console.log("res ?", Buffer.from(res.bytes).toString("hex"))

      // let benefactorSignature = privKey.sign(lockingTx.toCbor)
      // lockingTx.addSignatures(benefactorSignature)

      // await blockFrost.submitTx(lockingTx)

      // await network.submitTx(lockingTx)
      //network.tick(BigInt(100))

      //console.log(Date.now())

      // let pubHex = privKey.derivePubKey().hex
      // let epoch = await blockFrost.getLatestEpoch()
      return
    } catch (e) {
      console.error(e)
    }
  }
  useFocusEffect(
    React.useCallback(() => {
      run()
    }, [])
  )

  return (
    <MyCalendarProvider>
      {/*
      <ErrorHandler>
        <Calendar isRegularCalendar={true} isHomeScreen={true} navigateCb={navigateCb} />
      </ErrorHandler>
      */}
    </MyCalendarProvider>
  )
}
