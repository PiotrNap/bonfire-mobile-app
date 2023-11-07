import * as React from "react"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"

import { NavigationTabBar } from "components/navBarComponents/navigationTabBar"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { NavigationTabParamList } from "common/types/navigationTypes"
import {
  ProfileScreenStack,
  MyEventsStack,
  WalletScreenStack,
  BrowseScreensStack,
  CalendarScreenStack,
} from "stacks/index"

const NavigationTabs = createBottomTabNavigator<NavigationTabParamList>()

export const NavigationScreens = ({ route }: any) => {
  const { setID, setUsername, setHourlyRateAda } = React.useContext(ProfileContext)
  const { setUserSettings, bottomNavigationHeight } = appContext()

  React.useEffect(() => {
    // if the params aren't empty, we are redirected from
    // main screens stack during login
    if (Object.entries(route.params).length != 0) {
      const { username, id, hourlyRateAda, userSettings } = route.params

      id && setID(id)
      username && setUsername(username)
      hourlyRateAda && setHourlyRateAda(hourlyRateAda)
      setUserSettings(userSettings)
    }
  }, [])

  return (
    <NavigationTabs.Navigator
      sceneContainerStyle={
        {
          // paddingBottom: bottomNavigationHeight ? bottomNavigationHeight : 0,
        }
      }
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
