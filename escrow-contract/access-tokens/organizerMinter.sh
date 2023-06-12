#!/usr/bin/env bash

export CARDANO_NODE_SOCKET_PATH=/home/james/hd2/cardano/testnet/db/node.socket
cardano-cli query tip --testnet-magic 1097911063
cardano-cli query protocol-parameters --testnet-magic 1097911063  --out-file protocol.json

# Parameters
export SENDER=$1
export SENDERKEY=$2
export ORGANIZER=$3
export TOKENNAME=$4

cardano-cli query utxo --testnet-magic 1097911063  --address $SENDER

echo "Which TXIN?"
read TXIN

# echo "Address to send Organizer Access Token:"
# read CONTRIBUTOR

# echo "Name of Access Token:"
# read TOKENNAME

echo "Thanks, using $TXIN"
echo "Minting $TOKENNAME"
echo "Sending to $ORGANIZER"

export POLICYID=0c930db0966a7456dfa21096261a1c5caa7599390b9125212ce48fce

# convert TOKENAME to HEX
TOKENHEXSTRING=$(xxd -pu <<< $TOKENNAME)
export TOKENHEX=${TOKENHEXSTRING::-2}

cardano-cli transaction build \
--alonzo-era \
--testnet-magic 1097911063 \
--tx-in $TXIN \
--tx-out $ORGANIZER+"2000000 + 1 ${POLICYID}.${TOKENHEX}" \
--mint "1 ${POLICYID}.${TOKENHEX}" \
--mint-script-file bonfire-organizer-access-policy.script \
--change-address $SENDER \
--protocol-params-file protocol.json \
--out-file mint-${TOKENNAME}.draft

cardano-cli transaction sign \
--signing-key-file $SENDERKEY \
--testnet-magic 1097911063 \
--tx-body-file mint-${TOKENNAME}.draft \
--out-file mint-${TOKENNAME}.signed

cardano-cli transaction submit \
--tx-file mint-${TOKENNAME}.signed \
--testnet-magic 1097911063
