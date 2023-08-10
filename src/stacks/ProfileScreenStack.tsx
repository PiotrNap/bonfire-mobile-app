import * as React from "react"
import { createStackNavigator } from "@react-navigation/stack"

import { ProfileStackParamList } from "common/types/navigationTypes"
import {
  UserProfileScreen,
  UserProfileSettings,
  UserProfileEdit,
} from "screens/user"

const ProfileStack = createStackNavigator<ProfileStackParamList>()

export const ProfileScreenStack = () => {
  return (
    <ProfileStack.Navigator headerMode="none">
      <ProfileStack.Screen name="Profile Main" component={UserProfileScreen} />
      <ProfileStack.Screen name="Edit Profile" component={UserProfileEdit} />
      <ProfileStack.Screen
        name="Profile Settings"
        component={UserProfileSettings}
      />
    </ProfileStack.Navigator>
  )
}
