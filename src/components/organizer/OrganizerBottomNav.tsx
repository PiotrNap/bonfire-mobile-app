import * as React from "react"
import { NavigationContainer } from "@react-navigation/native"
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs"
import { HomeScreen, MyCalendarScreen, BrowseScreen } from "screens/index"
import { BrowseScreensStack } from "stacks/index"

import { appContext } from "contexts/contextApi"
import { WalletTopUpScreen } from "screens/onboarding"
import { UserProfileScreen } from "screens/user"
import { WalletScreen } from "screens/wallet/WalletScreen"

export interface OrganizerBottomNavProps {}

const Tab = createBottomTabNavigator()

export const OrganizerBottomNav = () => {
  const { auth } = appContext()

  const tabBarOptions = {
    // change color on click, etc...
  }

  return (
    <NavigationContainer>
      <Tab.Navigator tabBarOptions={tabBarOptions}>
        {auth ? (
          <>
            <Tab.Screen name="Home" component={HomeScreen} />
            <Tab.Screen name="Browse" component={BrowseScreensStack} />
            <Tab.Screen name="Wallet" component={WalletScreen} />
            <Tab.Screen name="Wallet/Add Funds" component={WalletTopUpScreen} />
            <Tab.Screen name="Availability" component={MyCalendarScreen} />
            <Tab.Screen name="Profile" component={UserProfileScreen} />
          </>
        ) : (
          <>
            <Tab.Screen name="Home" component={HomeScreen} />
            <Tab.Screen name="Browse" component={BrowseScreen} />
            <Tab.Screen name="Wallet" component={WalletScreen} />
            <Tab.Screen name="Profile" component={UserProfileScreen} />
          </>
        )}
      </Tab.Navigator>
    </NavigationContainer>
  )
}
