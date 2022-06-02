# Bonfire Escrow
## Plutus Documentation
### June 2022

# VO Testing
There are 2 Contracts:
1. Dispute:         `addr_test1wz3n674cvzdptuc8yezgynxlct2rraqk8gtq83zpdyepzds8vr72y`
2. Bonfire Escrow:  `addr_test1wryd5eypwq9v3c30nyv88evhs6aap0jcaguh0cx6l6gxnsggqw8tx`

### For any instance of Bonfire, we must define the following tokens:
#### Dispute Contract
1. The `CurrencySymbol` for a Bonfire Admin Token that must be held by anyone settling a Dispute.
2. Some fungible "payment token". (Will be `gimbal` on `mainnet`; code can be refactored to remove payment tokens, or to handle many.)
3. The `PubKeyHash` (or better, `ValidatorHash`) for a Bonfire Treasury. Can be a wallet address to start; a multi-sig contract will be better.

#### Escrow Contract
1. The `CurrencySymbol` for an Organizer Access Token
2. The same fungible "payment token" as specified in Dispute. (Will be `gimbal` on `mainnet`; code can be refactored to remove payment tokens, or to handle many.)
3. `ValidatorHash` of Dispute Contract
4. The same Bonfire Treasury PKH specified in Dispute Contract

### For Testing on `cardano-cli`:
- See `/output/bonfire-testnet`
- Two Access Token minting policies.
    - Address `addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql` owns the minting policy for these Access Tokens
    - Bonfire Admin Token `CurrencySymbol`: `0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce`
    - Organizer Access Token `CurrencySymbol`: `0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce`
- Minted 1,000,000,000 `bonGimbal` tokens minted in GameChanger Testnet, which allows us to test that decimals are correctly handles on FE: `982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c`
- New testnet wallet; `.skey` can be shared

## `DisputeParams`
```
bonfireAdminToken = "0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce",
dpPtSymbol = "982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e",
dpPtName = "bonGimbal",
bonfireTreasuryPkh = "f83d6f9d63a4b9541ad4efca5b48280bffdb8ac4e424c94432788109"
```

## `BonfireParams`
```
organizerAccessSymbol = "0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce"
ptSymbol    = "982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e"
ptName      = "bonGimbal"
treasuryPkh = "f83d6f9d63a4b9541ad4efca5b48280bffdb8ac4e424c94432788109"
disputeContract = "a33d7ab8609a15f3072644824cdfc2d431f4163a1603c44169321136"
```

## We need to test these Transactions:
1. The process always starts with an ATTENDEE locking one UTXO in the BonfireEscrow Contract
2. We must test 4 ways to unlock this UTXO:
    - Attendee can CANCEL 24 hours before event
    - Organizer can CANCEL any time before event
    - Organizer can COMPLETE an event 60 minutes after start time
    - Attendee can DISPUTE an event up to 60 minutes after start time
3. In every case except for Dispute, the interaction is complete.
4. If there's a Dispute, we must test the ways to unlock funds from Dispute Contract:
    - Pay Attendee
    - Pay Organizer
    - Split

### Things to Pay Attention To:
1. In any TX where we check for Access Token, we must also make sure that the token is sent back to Organizer. (This logic will change after Vasil fork.)
2. Test with and without Gimbals in Event payment

### Transaction #1: Attendee Books an Event --> Locks UTXO at BonfireEscrow Contract Addr:
- Step 1 - Set Variables:
```
TXIN=
SENDER=
SENDERKEY=
CONTRACT=
EVENTLOVELACE=
EVENTGIMBALS=
POLICYID=
TOKENNAME=
DATUMPATH=
```
- Step 2 - Construct Datum: Use `Escrow.BonfireDatum.hs`
- Step 3 - Build + Submit Tx:

```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN \
--tx-out $CONTRACT+"$EVENTLOVELACE + $EVENTGIMBALS" \
--tx-out-datum-embed-file $DATUMPATH \
--tx-out $SENDER+"" \
--change-address $SENDER \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063
```



### Test: Organizer Can Cancel an Event:
TXIN1=1ebe6ed12ea313c8f4128a88913b2e86a9cb3a9c48062f5bfdcd81241e54b616#1
TXIN2=0b7d781e9db52aa8672063b7219336cbe644a94e4b484f75e5bd1c473200501b#0
CONTRACTTXIN=de8bbb1c8a28392495a0672784945cba033112e3d41b748f1d3cbb654d693682#1
ATTENDEE=addr_test1vr3824f9g7w0au658pzngqu6r4gy6v2sfffas4hunq3hq3qpqegu5
EVENTLOVELACE=23000000
EVENTGIMBALS=1000000
ORGANIZERTOKEN="0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce.6a616d6573546573744f7267303031"
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
ORGANIZER

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-in $CONTRACTTXIN \
--tx-in-script-file bonfire-escrow-000.plutus \
--tx-in-datum-file mix-james-test-001.json \
--tx-in-redeemer-file OrgCancel.json \
--tx-in-collateral 6308cc82a08ec7a3c19ee0ca8e659f564dc39d5e37dcae3ba642c31d986e64fa#10 \
--tx-out $ATTENDEE+"$EVENTLOVELACE + $EVENTGIMBALS $PAYMENTTOKEN" \
--tx-out $ORGANIZER+"2000000 + 1 $ORGANIZERTOKEN" \
--change-address $ORGANIZER \
--required-signer-hash 22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3 \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ORGANIZERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063






















## Unlocking Event UTXOs
```
CONTRACTADDR=""
ATTENDEE=""
ORGANIZER=""
CONTRACTTXIN="#1"
ORGANIZERTXIN="#0"
COLLATERAL="#10"
```

```
cardano-cli transaction build \
--alonzo-era \
--tx-in $CONTRACTTXIN \
--tx-in-script-file bonfire-escrow-000.plutus \
--tx-in-datum-file piotr-attendee.json \
--tx-in-redeemer-file OrgCancel.json \
--tx-in $ORGANIZERTXIN \
--tx-in-collateral $COLLATERAL \
--tx-out $ATTENDEE+"4000000 + 20 cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059.706c6179" \
--required-signer-hash "22117fbd0f86a213ae4f4d824cd0d38eea29e49764ae22f5f50ba3d3" \
--change-address $ORGANIZER \
--protocol-params-file protocol.json \
--testnet-magic 1097911063 \
--out-file orgUnlock.raw

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file orgUnlock.raw \
--out-file orgUnlock.signed

cardano-cli transaction submit \
--tx-file orgUnlock.signed \
--testnet-magic 1097911063
```
### Organizer can Unlock any time

### Attendee can Unlock up to 24 hours before Event start time

## Some V1 Ideas:
- Implement multi-asset payment (V0 works with ADA and gimbals)
- Implement Organizer cancellation policy (V0, everyone works with same cancel policy)
- Implement Update as contract Action (V0 only has Cancel, Complete, Dispute)
- More robust Dispute handling! (Technically, V0 allows Attendee to Dispute too early)

## Things to think about:
- How does the Contract "reference" the records in the backend?
- How can users trust that information on backend is not changing?

---

## Quick Reference Simple TX
```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TX1 \
--tx-in $TX2 \
--tx-out $SENDER+"500000000 + 1000000000 982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c" \
--change-address $SENDER \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063
```