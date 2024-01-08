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

import { Confirmation, SuccessScreen } from "screens/payments"
import { NavigationScreens } from "tabs/NavigationScreens"
import { InitialUserScreens } from "tabs/InitialUserScreens"
import { useAppLogin } from "lib/hooks/useAppLogin"
import { authorizedLinkingConfig, unauthorizedLinkingConfig } from "lib/navigation"
import { LegalDocumentScreen } from "screens/LegalDocumentScreen"
import { ToastMessage } from "components/popups/toastMessage"
import { WelcomeScreen } from "screens/onboarding"

enableScreens() // enables native screens for navigation instead of using Views

// this will enable LayoutAnimation API
if (Platform.OS === "android") {
  if (UIManager.setLayoutAnimationEnabledExperimental) {
    UIManager.setLayoutAnimationEnabledExperimental(true)
  }
}
const Stack = createStackNavigator<AppStackParamList>()

function App() {
  const { isAuthorized, isAuthLoaded, user } = useAppLogin()
  // const onNavigationReady = () => SplashScreen.hide()

  // React.useEffect(() => {
  //   if (route.params?.["event-id"])
  //     (async () => await navigateToEvent(route.params?.["event-id"]))()
  // }, [])
  return (
    <>
      {isAuthLoaded && (
        <SafeAreaProvider>
          <AppContextProvider>
            <ProfileContextProvider>
              <WalletContextProvider>
                <NavigationContainer
                  linking={
                    isAuthorized ? authorizedLinkingConfig : unauthorizedLinkingConfig
                  }>
                  <Stack.Navigator
                    screenOptions={{
                      headerShown: false,
                    }}
                    initialRouteName={
                      !isAuthorized ? "Welcome Screen" : "Navigation Screens"
                    }>
                    <Stack.Screen
                      name="Welcome Screen"
                      component={WelcomeScreen}
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
      )}
      <ToastMessage />
    </>
  )
}

export default App
