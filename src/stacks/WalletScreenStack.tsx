import * as React from "react"
import { createStackNavigator } from "@react-navigation/stack"
import { WalletScreen } from "screens/wallet/WalletScreen"
import { WalletStackParamList } from "common/types/navigationTypes"
import { ReceiveTransactionScreen } from "screens/wallet/ReceiveTransactionScreen"
import { SendTransactionScreen } from "screens/wallet/SendTransactionScreen"
import { PreviewTransactionScreen } from "screens/PreviewTransactionScreen"
import { QrCodeScannerScreen } from "screens/QrCodeScannerScreen"

const WalletStack = createStackNavigator<WalletStackParamList>()

export const WalletScreenStack = () => {
  return (
    <WalletStack.Navigator headerMode="none" initialRouteName="Wallet Main">
      <WalletStack.Screen name={"Wallet Main"} component={WalletScreen} />
      <WalletStack.Screen
        name={"Receive Transaction"}
        component={ReceiveTransactionScreen}
      />
      <WalletStack.Screen name={"Send Transaction"} component={SendTransactionScreen} />
      <WalletStack.Screen
        name={"Preview Transaction"}
        component={PreviewTransactionScreen}
      />
      <WalletStack.Screen
        name={"Qr-Code Scanner"}
        options={{ headerShown: false, gestureEnabled: false }}
        component={QrCodeScannerScreen}
      />
    </WalletStack.Navigator>
  )
}
