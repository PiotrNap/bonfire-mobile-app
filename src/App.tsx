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

import * as Sentry from "@sentry/react-native"

import { setEra } from "@helios-lang/era"
setEra("Conway")

enableScreens() // enables native screens for navigation instead of using Views

// this will enable LayoutAnimation API
if (Platform.OS === "android") {
  if (UIManager.setLayoutAnimationEnabledExperimental) {
    UIManager.setLayoutAnimationEnabledExperimental(true)
  }
}
const Stack = createStackNavigator<AppStackParamList>()

Sentry.init({
  dsn: "https://d73dc53aa802e447540e8b2e29f633b0@o4508041102950400.ingest.us.sentry.io/4508041107472384",
  // Set tracesSampleRate to 1.0 to capture 100% of transactions for tracing.
  // We recommend adjusting this value in production.
  tracesSampleRate: 1.0,
  _experiments: {
    // profilesSampleRate is relative to tracesSampleRate.
    // Here, we'll capture profiles for 100% of transactions.
    profilesSampleRate: 1.0,
  },
})

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
          <ToastMessage />
        </SafeAreaProvider>
      )}
    </>
  )
}
export default Sentry.wrap(App)
