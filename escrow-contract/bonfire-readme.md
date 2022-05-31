# Bonfire Escrow
## Plutus Documentation
### June 2022

# VO Testing
There are 2 Contracts:
1. Dispute
2. Bonfire Escrow

### For any instance of Bonfire, we must define the following tokens:
#### Dispute Contract
1. The `CurrencySymbol` for a Bonfire Admin Token that must be held by anyone settling a Dispute.
2. Some fungible "payment token". (Will be `gimbal` on `mainnet`; code can be refactored to remove payment tokens, or to handle many.)
3. The `PubKeyHash` (or better, `ValidatorHash`) for a Bonfire Treasury. Can be a wallet address to start; a multi-sig contract will be better.

#### Escrow Contract
1. The `CurrencySymbol` for an Organizer Access Token
2. The same fungible "payment token" as specified in Dispute. (Will be `gimbal` on `mainnet`; code can be refactored to remove payment tokens, or to handle many.)
3. `ValidatorHash` of Dispute Contract

### For Testing on `cardano-cli`:
- See `/output/bonfire-testnet`
- Two Access Token minting policies.
    - Address `addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql` owns the minting policy for these Access Tokens
    - Bonfire Admin Token `CurrencySymbol`:
    - Organizer Access Token `CurrencySymbol`:
- Minted 1,000,000,000 `bonGimbal` tokens minted in GameChanger Testnet, which allows us to test that decimals are correctly handles on FE: `982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c`
- New testnet wallet; `.skey` can be shared





```
organizerAccessSymbol = ""
disputeContract = ""
ptSymbol    = "982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e"
ptName      = "bonGimbal"
```

## Set Variables:
```
CONTRACT=addr_test1wz2l0m99885whn9nk7pmeq46d0wgeunk3t2qrn3wkpz56cqautg0m
```

## Testnet Locking TX (Event "Creation"):
- Pay attention to Event start time, as specified in Datum.
- "Who" will be able to unlock this transaction "When"?
    - Attendee can cancel the Event up to 24 hours before event (funds back to Attendee)
    - Organizer can cancel any time (funds back to Attendee)
    - Organizer can claim UTXO 1 hour after event start time
    - Attendee can send the UTXO to Dispute Contract up until 1 hour after start time

```
```

```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN \
--tx-out $CONTRACT+"4000000 + 20 cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059.706c6179" \
--tx-out-datum-embed-file piotr-attendee.json \
--tx-out $SENDER+"5000000 + 4960 cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059.706c6179" \
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

## Unlocking Event UTXOs
```
CONTRACTADDR="addr_test1wz2l0m99885whn9nk7pmeq46d0wgeunk3t2qrn3wkpz56cqautg0m"
ATTENDEE="addr_test1vrszlrw40cmcaenn6klnhaezswp0zvd3we743zneeh38y6svxuvxr"
ORGANIZER="addr_test1qq3pzlaap7r2yyawfaxcynxs6w8w520yjaj2ugh4759685cv5qhzt95z9t8lur483fur90ge4ppqk2j89gmu8yy9m0ksac989n"
CONTRACTTXIN="86e1f9793b6b4d564f1a8cc9f864c05aca5a3463b11927d99aa257cab0a1c889#1"
ORGANIZERTXIN="8b90f54a3e751a5ca225d39f3466d89d6a5e5e73219e1a55c1c806b2ffc78d72#0"
COLLATERAL="6308cc82a08ec7a3c19ee0ca8e659f564dc39d5e37dcae3ba642c31d986e64fa#10"
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