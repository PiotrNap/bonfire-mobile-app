export default `
    spending escrow_contract

struct Datum {
    beneficiaryPkh : PubKeyHash
    benefactorPkh : PubKeyHash
    releaseDate : Time
    cancelFee: Int // % of paymentTokens
    cancelWindowStart: Time 
    cancelWindowEnd: Time
    createdAt : Time
    paymentTokens : Value
}

struct TxOutId {
    txId: TxId // hash of transaction
    utxoIdx: Int 
}

enum Redeemer { 
    Cancel {
        txId: TxId
        utxoIdx: Int
    }
    Complete {
        txOutIds: []TxOutId
    }
    Recycle
}

const TREASURY_PKH: ByteArray = #86de9a90be33e317c56ce862c737267dcd4a2daade25a4b7a7321acd
const treasuryPkh: PubKeyHash = PubKeyHash::new(TREASURY_PKH)

const BETA_TESTER_MPH: ByteArray = #481146d15d0c9bacc880254f88f944f6a88dba2e917d35fcbf92aa24
const betaTesterMph: MintingPolicyHash = MintingPolicyHash::new(BETA_TESTER_MPH)

const minServiceFee: Int = 1500000

func getServiceFeeAmnt(lovelacePayout: Int, withBetaTesterToken: Bool) -> Value {
    if (withBetaTesterToken) {
        Value::lovelace(0)
    } else if ( lovelacePayout / 100 < minServiceFee ) {
        Value::lovelace(minServiceFee)
    } else {
        Value::lovelace(lovelacePayout / 100)
    }
}

func checkInputForBetaTesterToken(txIn: TxInput) -> Bool {
    txIn.value.contains_policy(betaTesterMph)
}

/**
* This validator should meet the following requirements:
* 
*  1. Allow to cancel a transaction by one of two parties transacting with each other
*  2. Allow to cancel only before the cancellation deadline
*  3. Ensure all the funds are returned to benefactor during on-time cancellation 
*  4. Prevent from collecting funds before release date
*  5. Allow to collect funds after release date by beneficiary
*  6. Check whether sufficient fee was sent to the treasury (if any)
*  7. Allow to recycle UTxOs older than 1 year by the treasury
*/

func main(datum: Datum, redeemer: Redeemer, ctx: ScriptContext) -> Bool {
    tx: Tx = ctx.tx;
    benefactorPkh: PubKeyHash = datum.benefactorPkh;
    beneficiaryPkh: PubKeyHash = datum.beneficiaryPkh;
    now: Time = tx.time_range.start;
    betaTesterTokenPresent: Bool = tx.inputs.any(checkInputForBetaTesterToken);
    oneYearTime: Duration = Duration::new(1000 * 60 * 60 * 24 * 365);

    // How to handle 'txOutIds' with different beneficiary's or benefactor's ?

    redeemer.switch {
        cancelR: Cancel => {
            cancelWindow: TimeRange = TimeRange::new(datum.cancelWindowStart, datum.cancelWindowEnd);
            redeemerTxOutValue: Value = tx.inputs.find((in: TxInput) -> {
                in.output_id.index == cancelR.utxoIdx &&
                in.output_id.tx_id == cancelR.txId
            }).value;

            (datum.cancelWindowEnd > now).trace("Cancel1: ") && 

            if(cancelWindow.contains(now)) {
                if(tx.is_signed_by(benefactorPkh)) {
                    cFeeLove: Int = redeemerTxOutValue.get_lovelace() * (datum.cancelFee / 100);

                    // must return all payment tokens
                    (tx.value_sent_to(benefactorPkh) >= redeemerTxOutValue).trace("Cancel2") &&
                
                    if (cFeeLove < 2_000_000) {
                        (tx.value_sent_to(beneficiaryPkh).get_lovelace() >= 2_000_000).trace("Cancel3")
                    } else {
                        (tx.value_sent_to(beneficiaryPkh).get_lovelace() >= cFeeLove).trace("Cancel4")
                    }
                } else if (tx.is_signed_by(beneficiaryPkh)){
                    (tx.value_sent_to(benefactorPkh) >= redeemerTxOutValue).trace("Cancel5")
                } else {
                    false
                }
            } else {
                (tx.value_sent_to(benefactorPkh) >= redeemerTxOutValue).trace("Cancel6")
            }
        },
        completeR: Complete => {
            redeemerTxOutsValue: Value = tx.inputs.filter((txIn: TxInput) -> {
                                 completeR.txOutIds.any((txOutId: TxOutId) -> {
                                    txOutId.utxoIdx == txIn.output_id.index &&
                                    txOutId.txId == txIn.output_id.tx_id
                                    })
                                }).fold((acc: Value, txIn: TxInput) -> Value {
                                    acc + txIn.value
                                },Value::ZERO);
            serviceFee: Value = getServiceFeeAmnt(redeemerTxOutsValue.get_lovelace(), betaTesterTokenPresent);

           (tx.is_signed_by(beneficiaryPkh)).trace("Cmplt1") &&

           (now > datum.releaseDate).trace("Cmplt2") &&

           (tx.value_sent_to(beneficiaryPkh) >= redeemerTxOutsValue)
           .trace("Cmplt3") &&

           (tx.value_sent_to(treasuryPkh) == serviceFee)
           .trace("Cmplt4")
        },
        Recycle => {
           (tx.is_signed_by(treasuryPkh)).trace("Recycle1") &&

           (now > datum.createdAt + oneYearTime).trace("Recycle2")
        }
    }
}
`
