import * as React from "react"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"

import { TabParamList } from "common/types/navigationTypes"
import { NavigationTabBar } from "components/navBarComponents/navigationTabBar"
import { BrowseScreensStack } from "stacks/BrowseScreensStack"
import { HomeScreenStack } from "stacks/HomeScreenStack"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { ProfileScreenStack } from "stacks/ProfileScreenStack"
import { MyEventsStack } from "stacks/MyEventsStack"
import { WalletScreenStack } from "stacks/WalletScreenStack"

const NavigationTabs = createBottomTabNavigator<TabParamList>()

export const NavigationScreens = ({ route }: any) => {
  const { setId, setHourlyRate, setUsername, setProfileType } =
    React.useContext(ProfileContext)
  const { toggleAuth, setUserSettings } = appContext()

  React.useEffect(() => {
    // if the params aren't empty, we are redirected from
    // main screens stack during login
    if (Object.entries(route.params).length != 0) {
      const { profileType, username, id, hourlyRate, userSettings } =
        route.params

      profileType && toggleAuth(true, profileType)
      id && setId(id)
      profileType && setProfileType(profileType)
      username && setUsername(username)
      hourlyRate &&
        setHourlyRate({
          ada: hourlyRate?.ada || 0,
          gimbals: hourlyRate?.gimbals || 0,
        })
      setUserSettings(userSettings)
    }
  }, [])

  return (
    <NavigationTabs.Navigator
      screenOptions={{ headerShown: false }}
      //@ts-ignore
      tabBar={(props) => <NavigationTabBar {...props} />}>
      <NavigationTabs.Screen name="Home Stack" component={HomeScreenStack} />
      <NavigationTabs.Screen
        name="Browse Stack"
        component={BrowseScreensStack}
      />
      <NavigationTabs.Screen name="Wallet" component={WalletScreenStack} />
      <NavigationTabs.Screen name="My Events" component={MyEventsStack} />
      <NavigationTabs.Screen name="Profile" component={ProfileScreenStack} />
    </NavigationTabs.Navigator>
  )
}
