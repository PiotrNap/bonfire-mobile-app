import "react-native-gesture-handler";
import * as React from "react";
import { LogBox, Platform, UIManager } from "react-native";
import "./global";

import { NavigationContainer } from "@react-navigation/native";
import { createStackNavigator } from "@react-navigation/stack";
import { AppStackParamList } from "common/types/navigationTypes";
import { AppContextProvider } from "contexts/appContext";
import { ProfileContextProvider } from "contexts/profileContext";
// import {useFonts} from 'expo-font';
import { jsErrorHandler } from "lib/errors";
import { setJSExceptionHandler } from "react-native-exception-handler";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { enableScreens } from "react-native-screens";
import { WalletTopUpScreen } from "screens/onboarding";
import { Confirmation, DepositSuccessful } from "screens/payments";
import { NavigationScreens } from "tabs/NavigationScreens";
import { OnboardingScreens } from "tabs/OnboardingScreens";
import { UserRegistrationScreens } from "tabs/UserRegistrationScreens";
import { useAppLogin } from "lib/hooks/useAppLogin";
// import { LogIn } from "screens/LogIn";
// import AppLoading from 'expo-app-loading';

// Ignore all log notifications:
// LogBox.ignoreAllLogs();

setJSExceptionHandler(jsErrorHandler, true); // true - enables the error in dev mode
enableScreens(); // enable native screens for navigation instead of using Views

// this will enable LayoutAnimation API
if (Platform.OS === "android") {
  if (UIManager.setLayoutAnimationEnabledExperimental) {
    UIManager.setLayoutAnimationEnabledExperimental(true);
  }
}
const Stack = createStackNavigator<AppStackParamList>();

function App() {
  const { isAuthorized, isAuthLoaded, user } = useAppLogin();

  // const [fontsLoadaed] = useFonts({
  //   'Roboto-Thin': require('./assets/fonts/Roboto-Thin.ttf'),
  //   'Roboto-Light': require('./assets/fonts/Roboto-Light.ttf'),
  //   'Roboto-Regular': require('./assets/fonts/Roboto-Regular.ttf'),
  //   'Roboto-Medium': require('./assets/fonts/Roboto-Medium.ttf'),
  //   'Roboto-Bold': require('./assets/fonts/Roboto-Bold.ttf'),
  //   'Roboto-Black': require('./assets/fonts/Roboto-Black.ttf'),
  // });

  // if (!fontsLoadaed || !isAuthLoaded) {
  //   // return <AppLoading />;
  // } else {

  return (
    <SafeAreaProvider>
      <AppContextProvider>
        <ProfileContextProvider>
          <NavigationContainer>
            <Stack.Navigator
              initialRouteName={
                isAuthorized ? "Navigation Screens" : "Onboarding Screens"
              }
              headerMode="screen">
              {/*<Stack.Screen
                      name="Log In"
                      component={LogIn}
                      options={{ headerShown: false }}
                    />*/}
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
                name="Navigation Screens"
                component={NavigationScreens}
                options={{ headerShown: false }}
                initialParams={user}
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
  );
}

export default App;
