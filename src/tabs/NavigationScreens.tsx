import * as React from "react"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"

import { HomeScreen } from "screens/index"
import { OrganizerTabParamList } from "common/types/navigationTypes"
import { WalletScreen } from "screens/index"
import { NavigationTabBar } from "components/navBarComponents/navigationTabBar"
import { BrowseScreensStack } from "../stacks/BrowseScreensStack"
import { OrganizerHomeScreenStack } from "stacks/OrganizerHomeScreenStack"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { ProfileScreenStack } from "stacks/ProfileScreenStack"
import { MyEvents } from "screens/organizer/MyEvents"

const NavigationTabs = createBottomTabNavigator<OrganizerTabParamList>()

export const NavigationScreens = ({ route }: any) => {
  const { setId, setUsername, setProfileType } =
    React.useContext(ProfileContext)
  const { accountType, toggleAuth } = appContext()

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
    <NavigationTabs.Navigator
      //@ts-ignore
      tabBar={(props) => <NavigationTabBar {...props} />}>
      {accountType === "attendee" ? (
        <>
          <NavigationTabs.Screen name="Home" component={HomeScreen} />
          <NavigationTabs.Screen name="Browse" component={BrowseScreensStack} />
          <NavigationTabs.Screen name="Wallet" component={WalletScreen} />
          <NavigationTabs.Screen
            name="Profile"
            component={ProfileScreenStack}
          />
        </>
      ) : (
        <>
          <NavigationTabs.Screen
            name="Home"
            component={OrganizerHomeScreenStack}
          />
          <NavigationTabs.Screen name="Browse" component={BrowseScreensStack} />
          <NavigationTabs.Screen name="Wallet" component={WalletScreen} />
          <NavigationTabs.Screen name="My Events" component={MyEvents} />
          <NavigationTabs.Screen
            name="Profile"
            component={ProfileScreenStack}
          />
        </>
      )}
    </NavigationTabs.Navigator>
  )
}
