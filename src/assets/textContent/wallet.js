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
    import_mnemonics: {
      header: "Import an Existing Wallet",
      body: "Please insert each word of your recovery phrase into boxes below. Words are case sensitive and should be lower case.",
    },
    address_confirmation: {
      header: "Confirm Your Wallet Address",
      body: "Please confirm that given base address is known to you. If it's not, your previous wallet may be using a different method of address derivation. Or, you've inserted wrongly your recovery phrase.",
    },
  },
  create_wallet: {
    mnemonic_info_modal: {
      header: "Important Information",
      body: "You will be shown a set of 15 random words which are your secret recovery phrase (aka mnemonic phrase). Make sure you make a copy of it, either a virtual or a physical one. Anyone with access to this secret gains control over your wallet. Make sure no one is spying you!",
    },
    mnemonic_preview: {
      header: "Wallet Recovery Phrase",
      body: "Make sure you've made a backup copy of it. You will be able to reconstruct your wallet in a different Cardano wallet application (if it supports the same address derivation method).",
      checkbox_text:
        "I confirm making a backup copy of my secret recovery phrase",
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
