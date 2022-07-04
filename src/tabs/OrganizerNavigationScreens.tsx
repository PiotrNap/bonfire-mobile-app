import * as React from "react"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"

import { OrganizerTabParamList } from "common/types/navigationTypes"
import { NavigationTabBar } from "components/navBarComponents/navigationTabBar"
import { BrowseScreensStack } from "stacks/BrowseScreensStack"
import { OrganizerHomeScreenStack } from "stacks/OrganizerHomeScreenStack"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { ProfileScreenStack } from "stacks/ProfileScreenStack"
import { MyEventsStack } from "stacks/MyEventsStack"
// import { WalletScreenStack } from "stacks/WalletScreenStack"
import { WalletScreen } from "screens/WalletScreen"

const OrganizerNavigationTabs =
  createBottomTabNavigator<OrganizerTabParamList>()

export const OrganizerNavigationScreens = ({ route }: any) => {
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
      typeof hourlyRate === "number"
        ? setHourlyRate({ ada: hourlyRate, gimbals: 0 })
        : setHourlyRate(hourlyRate)
      setUserSettings(userSettings)
    }
  }, [])

  return (
    <OrganizerNavigationTabs.Navigator
      //@ts-ignore
      tabBar={(props) => <NavigationTabBar {...props} />}>
      <OrganizerNavigationTabs.Screen
        name="Home Stack"
        component={OrganizerHomeScreenStack}
      />
      <OrganizerNavigationTabs.Screen
        name="Browse Stack"
        component={BrowseScreensStack}
      />
      <OrganizerNavigationTabs.Screen name="Wallet" component={WalletScreen} />
      <OrganizerNavigationTabs.Screen
        name="My Events"
        component={MyEventsStack}
      />
      <OrganizerNavigationTabs.Screen
        name="Profile"
        component={ProfileScreenStack}
      />
    </OrganizerNavigationTabs.Navigator>
  )
}
