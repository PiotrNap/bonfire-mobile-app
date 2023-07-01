import * as React from "react"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"

import { OrganizerTabParamList } from "common/types/navigationTypes"
import { NavigationTabBar } from "components/navBarComponents/navigationTabBar"
import { BrowseScreensStack } from "../stacks/BrowseScreensStack"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { ProfileScreenStack } from "stacks/ProfileScreenStack"
import { WalletScreen } from "screens/wallet/WalletScreen"
import { HomeScreenStack } from "stacks/HomeScreenStack"

const AttendeeNavigationTabs = createBottomTabNavigator<OrganizerTabParamList>()

export const AttendeeNavigationScreens = ({ route }: any) => {
  const { setId, setUsername, setProfileType } =
    React.useContext(ProfileContext)
  const { toggleAuth } = appContext()

  React.useEffect(() => {
    // if the params aren't empty, we are redirected from
    // main screens stack during login
    if (Object.entries(route.params).length != 0) {
      const { profileType, username, id } = route.params

      profileType && toggleAuth(true, profileType)
      id && setId(id)
      profileType && setProfileType(profileType)
      username && setUsername(username)
    }
  }, [])

  return (
    <AttendeeNavigationTabs.Navigator
      screenOptions={{ headerShown: false }}
      //@ts-ignore
      tabBar={(props) => <NavigationTabBar {...props} />}>
      <AttendeeNavigationTabs.Screen
        name="Home Stack"
        component={HomeScreenStack}
      />
      <AttendeeNavigationTabs.Screen
        name="Browse Stack"
        component={BrowseScreensStack}
      />
      <AttendeeNavigationTabs.Screen name="Wallet" component={WalletScreen} />
      <AttendeeNavigationTabs.Screen
        name="Profile"
        component={ProfileScreenStack}
      />
    </AttendeeNavigationTabs.Navigator>
  )
}
