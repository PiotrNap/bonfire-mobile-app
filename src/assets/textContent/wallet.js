export const wallet = {
  add_new_wallet: {
    modal: {
      body: "Create a personal Bonfire wallet to book events and make payments. Or, import an existing wallet by providing a recovery phrase. You’re almost there.",
      header: "It looks like you haven't created a wallet yet",
      button_title: "Import",
      secondButton_title: "Create New",
    },
  },
  import_wallet: {
    address_confirmation: {
      header: "Confirm Your Credentials",
      // body: "Please confirm that the provided base address and username are familiar to you. If they are not, there are two possibilities:",
      body: "Please confirm that the provided base address and username are familiar to you.",
      first_bullet:
        "Your previous wallet may be using a different method for address derivation.",
      snd_bullet: "You may have entered your recovery phrase incorrectly.",
    },
  },
  create_wallet: {
    mnemonic_info_modal: {
      header: "Important Information",
      body: "You will be shown a set of 15 random words which are your secret recovery phrase (aka mnemonic phrase). Make sure you make a copy of it, either a virtual or a physical one. Anyone with access to this secret gains control over your wallet. Make sure no one is spying you!",
    },
    mnemonic_preview: {
      header: "Your Recovery Phrase",
      body: "Please store a copy of this somewhere secure. If you lose this phrase, you will not be able to reconstruct your account and access funds. Bonfire doesn't take any responsibility in case of a loss.",
      checkbox_text: "I have made a back up copy",
    },
    mnemonic_confirmation: {
      header: "Recovery Phrase Confirmation",
      body: "Please insert each word of your recovery phrase into boxes below. Words are case sensitive and should be lower case.",
    },
  },
  common: {
    wallet_set_up: {
      header: "Set Up a New Wallet",
      body: "Create a unique spending password. Make sure to make a backup copy of it!",
      body_add:
        "This password will give you access to your mnemonic phrase and allow you to spend your crypto assets.",
    },
  },
  risk_acknowledgement: {
    header: "Risk Acknowledgement",
    body_items: [
      {
        text: "Your private keys are stored on this device and not on Bonfires servers. We will never ask you to reveal your mnemonic phrase or any other secrets associated with your wallet.",
      },
      {
        text: "If the device on which you initialized or imported a wallet has been lost, the only way to restore your previous wallet is by re-entering your recovery phrase. The same rule applies for when uninstalling Bonfire application after a wallet has been created.",
      },
    ],
  },
}
