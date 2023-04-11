import "react-native-gesture-handler"
import "./global"

import * as React from "react"
import { LogBox, Platform, UIManager } from "react-native"

import { NavigationContainer } from "@react-navigation/native"
import { createStackNavigator } from "@react-navigation/stack"
import { AppStackParamList } from "common/types/navigationTypes"
import { AppContextProvider } from "contexts/appContext"
import { ProfileContextProvider } from "contexts/profileContext"
import { SafeAreaProvider } from "react-native-safe-area-context"
import { enableScreens } from "react-native-screens"
import SplashScreen from "react-native-splash-screen"

import { WalletTopUpScreen } from "screens/onboarding"
import { Confirmation, DepositSuccessful } from "screens/payments"
import { AttendeeNavigationScreens } from "tabs/AttendeeNavigationScreens"
import { OrganizerNavigationScreens } from "tabs/OrganizerNavigationScreens"
import { OnboardingScreens } from "tabs/OnboardingScreens"
import { UserRegistrationScreens } from "tabs/UserRegistrationScreens"
import { useAppLogin } from "lib/hooks/useAppLogin"
import {
  getAuthorizedLinkingConfig,
  getUnauthorizedLinkingConfig,
} from "lib/navigation"
import { useFirebaseMessaging } from "lib/hooks/useFirebaseMessaging"

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
  const { token } = useFirebaseMessaging(user?.id, isAuthorized)
  const onNavigationReady = () => SplashScreen.hide()

  if (!isAuthLoaded) {
    return <></>
  } else {
    return (
      <SafeAreaProvider>
        <AppContextProvider>
          <ProfileContextProvider>
            <NavigationContainer
              linking={
                isAuthorized
                  ? getAuthorizedLinkingConfig(user.profileType)
                  : getUnauthorizedLinkingConfig()
              }
              onReady={onNavigationReady}>
              <Stack.Navigator
                initialRouteName={
                  isAuthorized
                    ? user.profileType === "organizer"
                      ? "Organizer Navigation Screens"
                      : "Attendee Navigation Screens"
                    : "Onboarding Screens"
                }
                headerMode="screen">
                <Stack.Screen
                  name="Onboarding Screens"
                  component={OnboardingScreens}
                  options={{
                    headerShown: false,
                  }}
                />
                <Stack.Screen
                  name="User Registration Screens"
                  component={UserRegistrationScreens}
                  options={{
                    headerShown: false,
                  }}
                />
                <Stack.Screen
                  name="Attendee Navigation Screens"
                  component={AttendeeNavigationScreens}
                  options={{ headerShown: false }}
                  initialParams={{ ...user, token }}
                />
                <Stack.Screen
                  name="Organizer Navigation Screens"
                  component={OrganizerNavigationScreens}
                  options={{ headerShown: false }}
                  initialParams={{ ...user, token }}
                />
                <Stack.Screen
                  name="Deposit Successful"
                  options={{ headerShown: false, gestureEnabled: false }}
                  component={DepositSuccessful}
                />
                <Stack.Screen
                  name="Confirmation"
                  options={{ headerShown: false, gestureEnabled: false }}
                  component={Confirmation}
                />
                <Stack.Screen
                  name="Add Funds"
                  options={{ headerShown: false, gestureEnabled: false }}
                  component={WalletTopUpScreen}
                />
              </Stack.Navigator>
            </NavigationContainer>
          </ProfileContextProvider>
        </AppContextProvider>
      </SafeAreaProvider>
    )
  }
}

export default App
