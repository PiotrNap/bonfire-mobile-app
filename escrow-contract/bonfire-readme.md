# Bonfire MVP
### April 2022

## Some V1 Ideas:
- Implement multi-asset payment (V0 works with ADA and gimbals)
- Implement Organizer cancellation policy (V0, everyone works with same cancel policy)
- Implement Update as contract Action (V0 only has Cancel, Complete, Dispute)
- More robust Dispute handling! (Technically, V0 allows Attendee to Dispute too early)

## Things to think about:
- How does the Contract "reference" the records in the backend?
- How can users trust that information on backend is not changing?

## Things to look for in testing:
- Is it ok for the number of "pt" tokens to be zero? (Ie: only ADA is included in event cost?)


# VO Testing
```
organizerAccessSymbol = "61c8081a9aed827437c927074efb9ac07310d050256e587fc8b8cd83"
disputeContract = "9cb435a958e72371981537b57b178936d6d41b1fbd8cd1194f6a8014"
ptSymbol    = "cef5bfce1ff3fc5b128296dd0aa87e075a8ee8833057230c192c4059"
ptName      = "play"
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