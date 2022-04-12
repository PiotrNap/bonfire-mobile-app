import * as React from "react"
import { Alert, Platform, Pressable, StyleSheet, View } from "react-native"
import { StackScreenProps } from "@react-navigation/stack"
import RNRestart from "react-native-restart"

import { LeftArrowIcon } from "assets/icons"
import { Layout } from "components/layouts/basicLayout"

import { Colors, Sizing, Typography } from "styles/index"
import {
  appContext,
  bookingContext,
  eventCreationContext,
  myCalendarContext,
} from "contexts/contextApi"
import { ProfileStackParamList } from "common/types/navigationTypes"
import { SettingsItem } from "components/profile/settingsItem"
import { SmallDangerButton } from "components/buttons/smallDangerButton"
import { clearEncryptedStorage } from "lib/encryptedStorage"
import { ProfileContext } from "contexts/profileContext"

type ScreenProps = StackScreenProps<ProfileStackParamList, "Profile Settings">

export const UserProfileSettings = ({ navigation }: ScreenProps) => {
  const { colorScheme, resetAppState } = appContext()
  const { resetCalendarState } = myCalendarContext()
  const { resetBookingState } = bookingContext()
  const { resetEventCreationState } = eventCreationContext()

  const { resetProfileState } = React.useContext(ProfileContext)
  const isLightMode = colorScheme === "light"
  const isAndroid = Platform.OS === "android"

  const onBackNavigationPress = () => navigation.goBack()
  const removeStorageData = async () => {
    // removes everything from encrypted storage
    const success = await clearEncryptedStorage()

    if (!success) return showFailedModal()

    RNRestart.Restart()

    // clear state
    resetAppState()
    resetCalendarState()
    resetBookingState()
    resetEventCreationState()
    // user profile context has different structure..
    resetProfileState()
  }

  const showFailedModal = () => {
    Alert.alert(
      "Something went wrong",
      "We were unable to remove Bonfire data stored on this device. Please try again. If the problem persists please contact our support.",
      [
        {
          text: "Close",
          style: "cancel",
          onPress: () => {},
        },
      ],
      isAndroid ? { cancelable: true } : {}
    )
  }

  const showWarningModal = () => {
    Alert.alert(
      "Beware!",
      "This action is irreversible, you will loose access to your current wallet and account credentials on this device.",
      [
        {
          text: "Yes, I understand",
          style: "destructive",
          onPress: async () => await removeStorageData(),
        },
        {
          text: "Cancel",
          style: "cancel",
          onPress: () =>
            Alert.alert(
              "You are safe",
              "None of your credentials were removed. You can continue enjoying this application."
            ),
        },
      ],
      isAndroid ? { cancelable: true } : {}
    )
  }

  const { color: _, ...textStyle } = Typography.header.x20

  return (
    <Layout scrollable>
      <View style={styles.navigation}>
        <Pressable onPress={onBackNavigationPress} hitSlop={10}>
          <LeftArrowIcon
            width={24}
            height={24}
            color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
          />
        </Pressable>
      </View>
      <SettingsItem
        titleStyle={textStyle}
        title={
          "Remove all stored application data \n(includes private & public keys)."
        }>
        <SmallDangerButton onPressCallback={showWarningModal} text="Delete" />
      </SettingsItem>
    </Layout>
  )
}

const styles = StyleSheet.create({
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
})
