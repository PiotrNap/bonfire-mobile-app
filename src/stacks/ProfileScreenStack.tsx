import * as React from "react";
import { createStackNavigator } from "@react-navigation/stack";

import { UserProfileEditScreen } from "screens/profile/profileEdit/UserProfileEditScreen";
import { UserProfileScreen } from "screens/organizer/UserProfileScreen";
import { ProfileStackParamList } from "common/types/navigationTypes";

const ProfileStack = createStackNavigator<ProfileStackParamList>();

export const ProfileScreenStack = () => {
  return (
    <ProfileStack.Navigator headerMode="none">
      <ProfileStack.Screen name="Profile" component={UserProfileScreen} />
      <ProfileStack.Screen
        name="Edit Profile"
        component={UserProfileEditScreen}
      />
    </ProfileStack.Navigator>
  );
};
