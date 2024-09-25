import * as React from "react"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"

import { NavigationTabBar } from "components/navBarComponents/navigationTabBar"
import { appContext, walletContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { NavigationTabParamList } from "common/types/navigationTypes"
import {
  ProfileScreenStack,
  MyEventsStack,
  WalletScreenStack,
  BrowseScreensStack,
  CalendarScreenStack,
} from "stacks/index"
import { useSafeAreaInsets } from "react-native-safe-area-context"
import { getSupportedBiometryType } from "react-native-keychain"

const NavigationTabs = createBottomTabNavigator<NavigationTabParamList>()

export const NavigationScreens = ({ route }: any) => {
  const { setID, setUsername, setHourlyRateAda, setTimeZone, setCollateralUtxoId } =
    React.useContext(ProfileContext)
  const { setBaseAddresses } = walletContext()
  const { setUserSettings, setDeviceTopInsent, setBiometryType } = appContext()
  const insets = useSafeAreaInsets()

  React.useEffect(() => {
    ;(async () => {
      let t = await getSupportedBiometryType()
      setBiometryType(t)
    })()
    // if the params aren't empty, we are redirected from
    // main screens stack during login
    if (Object.entries(route.params).length != 0) {
      const {
        username,
        id,
        hourlyRateAda,
        userSettings,
        timeZone,
        addresses,
        collateralUtxoId,
      } = route.params

      id && setID(id)
      username && setUsername(username)
      hourlyRateAda && setHourlyRateAda(hourlyRateAda)
      setUserSettings(userSettings)
      setTimeZone(timeZone)
      setBaseAddresses(addresses)
      setCollateralUtxoId(collateralUtxoId)
      setDeviceTopInsent(insets.top)
    }
  }, [])

  return (
    <NavigationTabs.Navigator
      screenOptions={{ headerShown: false }}
      tabBar={(props) => <NavigationTabBar {...props} />}>
      <NavigationTabs.Screen name="Wallet" component={WalletScreenStack} />
      <NavigationTabs.Screen name="Browse" component={BrowseScreensStack} />
      <NavigationTabs.Screen name="My Events" component={MyEventsStack} />
      <NavigationTabs.Screen name="Calendar" component={CalendarScreenStack} />
      <NavigationTabs.Screen name="Profile" component={ProfileScreenStack} />
    </NavigationTabs.Navigator>
  )
}
