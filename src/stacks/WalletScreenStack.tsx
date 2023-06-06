import * as React from "react"
import { createStackNavigator } from "@react-navigation/stack"
import { WalletScreen } from "screens/wallet/WalletScreen"
import { WalletStackParamList } from "common/types/navigationTypes"
import { ImportMnemonicsScreen } from "screens/wallet/ImportMnemonicsScreen"
import { ImportMnemonicsConfirmation } from "screens/wallet/ImportMnemonicsConfirmationScreen"
import { NewWalletSetUp } from "screens/wallet/NewWalletSetUpScreen"
import { MnemonicsPreview } from "screens/wallet/MnemonicsPreviewScreen"

const WalletStack = createStackNavigator<WalletStackParamList>()

export const WalletScreenStack = () => {
  return (
    <WalletStack.Navigator headerMode="none" initialRouteName="Wallet">
      <WalletStack.Screen name={"Wallet"} component={WalletScreen} />
      <WalletStack.Screen
        name={"Import Mnemonics"}
        component={ImportMnemonicsScreen}
      />
      <WalletStack.Screen
        name={"Import Mnemonics Confirmation"}
        component={ImportMnemonicsConfirmation}
      />
      <WalletStack.Screen
        name={"New Wallet Set Up"}
        component={NewWalletSetUp}
      />
      <WalletStack.Screen
        name={"Mnemonic Preview"}
        component={MnemonicsPreview}
      />
    </WalletStack.Navigator>
  )
}
