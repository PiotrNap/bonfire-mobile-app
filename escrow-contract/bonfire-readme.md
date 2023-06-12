# Bonfire Escrow
## Plutus Documentation
### June 2022

# VO Testing
There are 2 Contracts:
1. Dispute:         `addr_test1wqftm6cc67agdg30rssmk7fppq9ytwylng0fnv5wgwtljnsw2cf8n`
2. Bonfire Escrow:  `addr_test1wz4zy9gqw40ct28hw4r0e4gv4tg3gwf6fgftannn202pwvgd3mvzj`

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
disputeContract = "a6e6fe9543132a112ebd056779b727b0ff4d33ec76eb77a7d1ed21a0"
```

## Contract Addresses (V3 - updated 2022-06-20)
```
CONTRACT=addr_test1wq5p2ul7nk37e89w2s06deupes4kxua9swyqdm9hs9spqdsvuny2y
DISPUTECONTRACT=addr_test1wznwdl54gvfj5yfwh5zkw7dhy7c07nfna3mwkaa868kjrgqwmy5zc
```

## Testing Addresses
```
ADMIN=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
ORGANIZER=addr_test1qqdffdmhqh56zssx2hndj5h3x0u9ej63feaks9gwsnp2k7c7r7y2e9sq4acqxuhgz47hs8c3qyr3msyam60ntqyhs4uq3l9r89
ATTENDEE=addr_test1qz9dgcjnam9lwvhszufm77997ldg5de5xmyd6s40q9vjqc4st24qnf0rynsma7ajvnp7nc8y8vpdgmchecl8ug503assug2qrs
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

TXIN1=8edd053e069bb4fc3df72c8059989b504e5c6cbe4b2af6e2976af1974efc283a#2
TXIN2=91eaaca26d73b0264ad5e69c98860d5dea7528b1f5b186dd9fba272de5afa49d#0
CONTRACT=addr_test1wz4zy9gqw40ct28hw4r0e4gv4tg3gwf6fgftannn202pwvgd3mvzj
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-datum-2.json
```
- Step 2 - Construct Datum: Use `Escrow.BonfireDatum.hs`
- Step 3 - Build + Submit Tx:

```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $CONTRACT+"$EVENTLOVELACE + $EVENTGIMBALS $PAYMENTTOKEN" \
--tx-out-datum-embed-file $DATUMPATH \
--tx-out $ATTENDEE+"5000000 + 1850000 $PAYMENTTOKEN" \
--change-address $ATTENDEE \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ATTENDEEKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063
```
### Test: Attendee Can Cancel an Event up to 24 hours before deadline:
TXIN1=abb0534a9122616127e9cc87bcc543a080979678ec958fd85494e758d01336d0#0
CONTRACTTXIN=abb0534a9122616127e9cc87bcc543a080979678ec958fd85494e758d01336d0#1
ATTENDEE=addr_test1qz9dgcjnam9lwvhszufm77997ldg5de5xmyd6s40q9vjqc4st24qnf0rynsma7ajvnp7nc8y8vpdgmchecl8ug503assug2qrs
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
COLLATERAL=8edd053e069bb4fc3df72c8059989b504e5c6cbe4b2af6e2976af1974efc283a#0
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/AttCancel.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-escrow-v2.plutus
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-datum-2.json
ATTENDEEPKH=8ad46253eecbf732f01713bf78a5f7da8a373436c8dd42af01592062

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ATTENDEE+"$EVENTLOVELACE + $EVENTGIMBALS $PAYMENTTOKEN" \
--change-address $ATTENDEE \
--required-signer-hash $ATTENDEEPKH \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ATTENDEEKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063


### Test: Organizer Can Cancel an Event:
TXIN1=53d0e7a10069425e5d1eb0c49ed73eeac421d81fff9f4392cdd94a6595c09525#1
TXIN2=54d9e29d9d1cd42062908027bb05e8f6c194d23b7e39b0435c267dfc59a0261e#1
CONTRACTTXIN=abb0534a9122616127e9cc87bcc543a080979678ec958fd85494e758d01336d0#1
ATTENDEE=addr_test1qz9dgcjnam9lwvhszufm77997ldg5de5xmyd6s40q9vjqc4st24qnf0rynsma7ajvnp7nc8y8vpdgmchecl8ug503assug2qrs
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
ORGANIZERTOKEN="0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce.6f7267616e697a657231"
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/OrgCancel.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-escrow-v2.plutus
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-datum-2.json
ORGANIZERPKH=1a94b77705e9a1420655e6d952f133f85ccb514e7b68150e84c2ab7b
COLLATERAL=54d9e29d9d1cd42062908027bb05e8f6c194d23b7e39b0435c267dfc59a0261e#4


cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ATTENDEE+"$EVENTLOVELACE + $EVENTGIMBALS $PAYMENTTOKEN" \
--tx-out $ORGANIZER+"2000000 + 1 $ORGANIZERTOKEN" \
--change-address $ORGANIZER \
--required-signer-hash $ORGANIZERPKH \
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

### Test: Organizer Collect by "Complete"ing event - any time 60+ minutes after start time.
TXIN1=53d0e7a10069425e5d1eb0c49ed73eeac421d81fff9f4392cdd94a6595c09525#1
TXIN2=54d9e29d9d1cd42062908027bb05e8f6c194d23b7e39b0435c267dfc59a0261e#1
CONTRACTTXIN=abb0534a9122616127e9cc87bcc543a080979678ec958fd85494e758d01336d0#1
ORGANIZER=addr_test1qqdffdmhqh56zssx2hndj5h3x0u9ej63feaks9gwsnp2k7c7r7y2e9sq4acqxuhgz47hs8c3qyr3msyam60ntqyhs4uq3l9r89
ORGANIZERTOKEN="0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce.6f7267616e697a657231"
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/Complete.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-escrow-v2.plutus
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-datum-2.json
ORGANIZERPKH=1a94b77705e9a1420655e6d952f133f85ccb514e7b68150e84c2ab7b
COLLATERAL=54d9e29d9d1cd42062908027bb05e8f6c194d23b7e39b0435c267dfc59a0261e#4
EVENTLOVELACE=23750000
EVENTGIMBALS=1425000
LOVELACETOTREAS=1250000
GIMBALSTOTREAS=75000

SLOT=61030331
Note: we can query tip for current slot and use that slot.
This is like saying "this tx is invalid before now"


cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ORGANIZER+"$EVENTLOVELACE + $EVENTGIMBALS $PAYMENTTOKEN" \
--tx-out $ORGANIZER+"2000000 + 1 $ORGANIZERTOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENTTOKEN" \
--change-address $ORGANIZER \
--invalid-before $SLOT \
--required-signer-hash $ORGANIZERPKH \
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

### Test: Attendee Can Dispute an Event
ATTENDEE
TXIN1=e8b0d97c9a0ea2a4e0e748f6dbc303cd3a5b24fbce4e6efe63619de29fa0fd22#0
CONTRACTTXIN=8edd053e069bb4fc3df72c8059989b504e5c6cbe4b2af6e2976af1974efc283a#1
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
UNLOCKESCROWDATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-datum.json
LOCKDISPUTEDATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-dispute-datum.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-escrow-v2.plutus
DISPUTECONTRACT=addr_test1wqftm6cc67agdg30rssmk7fppq9ytwylng0fnv5wgwtljnsw2cf8n
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/Dispute.json
COLLATERAL=8edd053e069bb4fc3df72c8059989b504e5c6cbe4b2af6e2976af1974efc283a#0
ATTENDEEPKH=8ad46253eecbf732f01713bf78a5f7da8a373436c8dd42af01592062

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $UNLOCKESCROWDATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $DISPUTECONTRACT+"$EVENTLOVELACE + $EVENTGIMBALS $PAYMENTTOKEN" \
--tx-out-datum-embed-file $LOCKDISPUTEDATUMPATH \
--change-address $ATTENDEE \
--required-signer-hash $ATTENDEEPKH \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ATTENDEEKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063


### Test: Resolving Disputes --> Pay to Attendee
TXINADMIN=1dac708b81ddc8233b229ebfa4f17292a8655eec4e9a63c4ecf98e1300e9e47f#3
TXINFEE=1dac708b81ddc8233b229ebfa4f17292a8655eec4e9a63c4ecf98e1300e9e47f#0
CONTRACTTXIN=91eaaca26d73b0264ad5e69c98860d5dea7528b1f5b186dd9fba272de5afa49d#1
ATTENDEE=addr_test1qz9dgcjnam9lwvhszufm77997ldg5de5xmyd6s40q9vjqc4st24qnf0rynsma7ajvnp7nc8y8vpdgmchecl8ug503assug2qrs
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-dispute-datum.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-dispute-v2.plutus
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/PayAttendee.json
COLLATERAL=c7a944e18f0b4e4b164d698b148b6436519a874794bb1542382bae2e5a038c06#0
ADMIN=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
TREASURY=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
ADMINTOKEN="0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce.7465737441646d696e303032"
LOVELACETOATT=22500000
GIMBALSTOATT=1350000
LOVELACETOTREAS=2500000
GIMBALSTOTREAS=150000

Note: in this test case, `ADMIN` and `TREASURY` are same addr, but they will not be in Dapp.

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXINADMIN \
--tx-in $TXINFEE \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ATTENDEE+"$LOVELACETOATT + $GIMBALSTOATT $PAYMENTTOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENTTOKEN" \
--tx-out $ADMIN+"2000000 + 1 $ADMINTOKEN" \
--change-address $ADMIN \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ADMINKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063



### Test: Resolving Disputes --> Pay to Organizer
TXINADMIN=1dac708b81ddc8233b229ebfa4f17292a8655eec4e9a63c4ecf98e1300e9e47f#3
TXINFEE=1dac708b81ddc8233b229ebfa4f17292a8655eec4e9a63c4ecf98e1300e9e47f#0
CONTRACTTXIN=91eaaca26d73b0264ad5e69c98860d5dea7528b1f5b186dd9fba272de5afa49d#1
ATTENDEE=addr_test1qz9dgcjnam9lwvhszufm77997ldg5de5xmyd6s40q9vjqc4st24qnf0rynsma7ajvnp7nc8y8vpdgmchecl8ug503assug2qrs
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-dispute-datum.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-dispute-v2.plutus
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/PayOrganizer.json
COLLATERAL=c7a944e18f0b4e4b164d698b148b6436519a874794bb1542382bae2e5a038c06#0
ADMIN=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
TREASURY=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
ADMINTOKEN="0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce.7465737441646d696e303032"
LOVELACETOORG=22500000
GIMBALSTOORG=1350000
LOVELACETOTREAS=2500000
GIMBALSTOTREAS=150000

Note: in this test case, `ADMIN` and `TREASURY` are same addr, but they will not be in Dapp.

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXINADMIN \
--tx-in $TXINFEE \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ORGANIZER+"$LOVELACETOORG + $GIMBALSTOORG $PAYMENTTOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENTTOKEN" \
--tx-out $ADMIN+"2000000 + 1 $ADMINTOKEN" \
--change-address $ADMIN \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ADMINKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063


### Test: Resolving Disputes --> Split
Note 16 June - Need to recompile contracts to fix 45/10 error.

```
TXINADMIN=1dac708b81ddc8233b229ebfa4f17292a8655eec4e9a63c4ecf98e1300e9e47f#3
TXINFEE=1dac708b81ddc8233b229ebfa4f17292a8655eec4e9a63c4ecf98e1300e9e47f#0
CONTRACTTXIN=91eaaca26d73b0264ad5e69c98860d5dea7528b1f5b186dd9fba272de5afa49d#1
ATTENDEE=addr_test1qz9dgcjnam9lwvhszufm77997ldg5de5xmyd6s40q9vjqc4st24qnf0rynsma7ajvnp7nc8y8vpdgmchecl8ug503assug2qrs
EVENTLOVELACE=25000000
EVENTGIMBALS=1500000
PAYMENTTOKEN="982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c"
DATUMPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/datums/example-dispute-datum.json
PLUTUSPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/plutus-scripts/bonfire-dispute-v2.plutus
REDEEMERPATH=/home/james/hd2/bonfire/bonfire/escrow-contract/redeemers/Split.json
COLLATERAL=c7a944e18f0b4e4b164d698b148b6436519a874794bb1542382bae2e5a038c06#0
ADMIN=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
TREASURY=addr_test1qrur6muavwjtj4q66nhu5k6g9q9llku2cnjzfj2yxfugzz0jvp30yy579zdq3fdgsyttt35m8s7mphjcmk6wugrdxp0qskpeql
ADMINTOKEN="0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce.7465737441646d696e303032"
LOVELACETOATT=11250000
GIMBALSTOATT=675000
LOVELACETOORG=11250000
GIMBALSTOORG=675000
LOVELACETOTREAS=2500000
GIMBALSTOTREAS=150000

Note: in this test case, `ADMIN` and `TREASURY` are same addr, but they will not be in Dapp.

cardano-cli transaction build \
--alonzo-era \
--tx-in $TXINADMIN \
--tx-in $TXINFEE \
--tx-in $CONTRACTTXIN \
--tx-in-script-file $PLUTUSPATH \
--tx-in-datum-file $DATUMPATH \
--tx-in-redeemer-file $REDEEMERPATH \
--tx-in-collateral $COLLATERAL \
--tx-out $ORGANIZER+"$LOVELACETOORG + $GIMBALSTOORG $PAYMENTTOKEN" \
--tx-out $ATTENDEE+"$LOVELACETOATT + $GIMBALSTOATT $PAYMENTTOKEN" \
--tx-out $TREASURY+"$LOVELACETOTREAS + $GIMBALSTOTREAS $PAYMENTTOKEN" \
--tx-out $ADMIN+"2000000 + 1 $ADMINTOKEN" \
--change-address $ADMIN \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $ADMINKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063
```







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
- Do we need to change the `geq` in BonfireDispute?

---

## Quick Tx
```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN \
--tx-out $ORGANIZER+20000000 \
--tx-out $ORGANIZER+20000000 \
--tx-out $ORGANIZER+20000000 \
--tx-out $ORGANIZER+20000000 \
--change-address $ORGANIZER \
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
```

## Quick Send bonGimbals TX
```
cardano-cli transaction build \
--alonzo-era \
--tx-in $TXIN1 \
--tx-in $TXIN2 \
--tx-out $ATTENDEE+"100000000 + 5000000 982ff92902a6d9c547506a9d53f342899857562f30f51c0232fb668e.626f6e47696d62616c" \
--change-address $MONDAY \
--protocol-params-file protocol.json \
--out-file tx.raw \
--testnet-magic 1097911063

cardano-cli transaction sign \
--signing-key-file $MONDAYKEY \
--testnet-magic 1097911063 \
--tx-body-file tx.raw \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 1097911063
```