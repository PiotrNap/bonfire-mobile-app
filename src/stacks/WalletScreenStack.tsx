import * as React from "react"
import { createStackNavigator } from "@react-navigation/stack"
import { WalletScreen } from "screens/wallet/WalletScreen"
import { WalletStackParamList } from "common/types/navigationTypes"
import { ImportMnemonicScreen } from "screens/wallet/ImportMnemonicScreen"
import { NewWalletSetUp } from "screens/wallet/NewWalletSetUpScreen"
import { MnemonicPreview } from "screens/wallet/MnemonicPreviewScreen"
import { ImportMnemonicConfirmation } from "screens/wallet/ImportMnemonicConfirmationScreen"

const WalletStack = createStackNavigator<WalletStackParamList>()

export const WalletScreenStack = () => {
  return (
    <WalletStack.Navigator headerMode="none" initialRouteName="Wallet Main">
      <WalletStack.Screen name={"Wallet Main"} component={WalletScreen} />
      <WalletStack.Screen
        name={"Import Mnemonic"}
        component={ImportMnemonicScreen}
      />
      <WalletStack.Screen
        name={"Import Mnemonic Confirmation"}
        component={ImportMnemonicConfirmation}
      />
      <WalletStack.Screen
        name={"New Wallet Set Up"}
        component={NewWalletSetUp}
      />
      <WalletStack.Screen
        name={"Mnemonic Preview"}
        component={MnemonicPreview}
      />
    </WalletStack.Navigator>
  )
}
