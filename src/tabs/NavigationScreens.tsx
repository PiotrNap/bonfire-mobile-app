import * as React from "react";
import { createBottomTabNavigator } from "@react-navigation/bottom-tabs";

import { HomeScreen } from "screens/index";
import { OrganizerTabParamList } from "common/types/navigationTypes";
import { WalletScreen } from "screens/index";
import { Calendar } from "containers/MyCalendar";
import { NavigationTabBar } from "components/navBarComponents/navigationTabBar";
import { BrowseScreensStack } from "../stacks/BrowseScreensStack";
import { OrganizerHomeScreenStack } from "stacks/OrganizerHomeScreenStack";
import { appContext } from "contexts/contextApi";
import { ProfileContext } from "contexts/profileContext";
import { ProfileScreenStack } from "stacks/ProfileScreenStack";

const NavigationTabs = createBottomTabNavigator<OrganizerTabParamList>();

export const NavigationScreens = ({ route }: any) => {
  const { setId, setUsername, setProfileType } =
    React.useContext(ProfileContext);
  const { accountType, toggleAuth } = appContext();

  React.useEffect(() => {
    // if the params aren't empty, we are redirected from main App during login
    if (route.params) {
      const { profileType, username, id } = route.params;

      toggleAuth(true, profileType);
      setId(id);
      setProfileType(profileType);
      setUsername(username);
    }
  }, []);

  return (
    <NavigationTabs.Navigator
      //@ts-ignore
      tabBar={(props) => <NavigationTabBar {...props} />}>
      <NavigationTabs.Screen
        name="Home"
        component={
          accountType === "attendee" ? HomeScreen : OrganizerHomeScreenStack
        }
      />
      <NavigationTabs.Screen name="Browse" component={BrowseScreensStack} />
      <NavigationTabs.Screen name="Wallet" component={WalletScreen} />
      <NavigationTabs.Screen name="My Events" component={Calendar} />
      <NavigationTabs.Screen name="Profile" component={ProfileScreenStack} />
    </NavigationTabs.Navigator>
  );
};
