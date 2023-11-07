import "react-native-gesture-handler"
import "../global"

import * as React from "react"
import { Platform, UIManager } from "react-native"

import { NavigationContainer } from "@react-navigation/native"
import { createStackNavigator } from "@react-navigation/stack"
import { AppStackParamList } from "common/types/navigationTypes"
import { AppContextProvider } from "contexts/appContext"
import { ProfileContextProvider } from "contexts/profileContext"
import { WalletContextProvider } from "contexts/walletContext"
import { SafeAreaProvider } from "react-native-safe-area-context"
import { enableScreens } from "react-native-screens"
import SplashScreen from "react-native-splash-screen"

import { QrCodeScannerScreen } from "screens/QrCodeScannerScreen"
import { Confirmation, SuccessScreen } from "screens/payments"
import { NavigationScreens } from "tabs/NavigationScreens"
import { OnboardingScreens } from "tabs/OnboardingScreens"
import { InitialUserScreens } from "tabs/InitialUserScreens"
import { useAppLogin } from "lib/hooks/useAppLogin"
import { authorizedLinkingConfig, unauthorizedLinkingConfig } from "lib/navigation"
import Toast, { ErrorToast } from "react-native-toast-message"
import { Typography } from "./styles"
import { LegalDocumentScreen } from "screens/LegalDocumentScreen"

// setJSExceptionHandler(jsErrorHandler, true); // true - enables the error in dev mode
enableScreens() // enable native screens for navigation instead of using Views

// TODO remove it
// LogBox.ignoreAllLogs()

// this will enable LayoutAnimation API
if (Platform.OS === "android") {
  if (UIManager.setLayoutAnimationEnabledExperimental) {
    UIManager.setLayoutAnimationEnabledExperimental(true)
  }
}
const Stack = createStackNavigator<AppStackParamList>()

function App() {
  const { isAuthorized, isAuthLoaded, user } = useAppLogin()
  // const { token } = useFirebaseMessaging(user?.id, isAuthorized)
  const onNavigationReady = () => SplashScreen.hide()

  // React.useEffect(() => {
  //   if (route.params?.["event-id"])
  //     (async () => await navigateToEvent(route.params?.["event-id"]))()
  // }, [])

  const toastConfig = {
    /*
    Overwrite 'error' type,
    by modifying the existing `ErrorToast` component
  */
    error: (props) => (
      <ErrorToast
        {...props}
        text1Style={{
          ...Typography.header.x25,
        }}
        text2Style={{
          ...Typography.body.x10,
        }}
      />
    ),
  }

  if (!isAuthLoaded) {
    return <></>
  } else {
    return (
      <>
        <SafeAreaProvider>
          <AppContextProvider>
            <ProfileContextProvider>
              <WalletContextProvider>
                <NavigationContainer
                  linking={
                    isAuthorized ? authorizedLinkingConfig : unauthorizedLinkingConfig
                  }
                  onReady={onNavigationReady}>
                  <Stack.Navigator
                    screenOptions={{
                      headerShown: false,
                    }}
                    initialRouteName={
                      !isAuthorized ? "Onboarding Screens" : "Navigation Screens"
                    }>
                    <Stack.Screen
                      name="Onboarding Screens"
                      component={OnboardingScreens}
                      options={{
                        headerShown: false,
                      }}
                    />
                    <Stack.Screen
                      name="Initial User Screens"
                      component={InitialUserScreens}
                      options={{
                        headerShown: false,
                      }}
                    />
                    <Stack.Screen
                      name="Navigation Screens"
                      component={NavigationScreens}
                      options={{ headerShown: false }}
                      initialParams={{ ...user }}
                    />
                    <Stack.Screen
                      name="Success"
                      options={{ headerShown: false, gestureEnabled: false }}
                      component={SuccessScreen}
                    />
                    <Stack.Screen
                      name="Confirmation"
                      options={{ headerShown: false, gestureEnabled: false }}
                      component={Confirmation}
                    />
                    <Stack.Screen
                      name="Qr-Code Scanner"
                      options={{ headerShown: false, gestureEnabled: false }}
                      component={QrCodeScannerScreen}
                    />
                    <Stack.Screen
                      name="Legal Document"
                      options={{ headerShown: false, gestureEnabled: false }}
                      component={LegalDocumentScreen}
                    />
                    {/*
                  <Stack.Screen
                    name="Add Funds"
                    options={{ headerShown: false, gestureEnabled: false }}
                    component={WalletTopUpScreen}
                  />
                  */}
                  </Stack.Navigator>
                </NavigationContainer>
              </WalletContextProvider>
            </ProfileContextProvider>
          </AppContextProvider>
        </SafeAreaProvider>
        <Toast config={toastConfig} />
      </>
    )
  }
}

export default App
