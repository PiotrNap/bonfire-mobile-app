import * as React from "react"
import { Pressable, StyleSheet, View } from "react-native"
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
import {
  showAccountDeletionWarningModal,
  showCredentialsWarningModal,
  showFailedModal,
} from "lib/modalAlertsHelpers"
import {
  FAILED_ACCOUNT_DELETION_MSG,
  FAILED_CREDENTIALS_DELETION_MSG,
} from "lib/errors"
import { Users } from "Api/Users"

type ScreenProps = StackScreenProps<ProfileStackParamList, "Profile Settings">

export const UserProfileSettings = ({ navigation }: ScreenProps) => {
  const { id } = React.useContext(ProfileContext)
  const { colorScheme, resetAppState } = appContext()
  const { resetCalendarState } = myCalendarContext()
  const { resetBookingState } = bookingContext()
  const { resetEventCreationState } = eventCreationContext()

  const { resetProfileState } = React.useContext(ProfileContext)
  const isLightMode = colorScheme === "light"

  const onBackNavigationPress = () => navigation.goBack()

  const removeStorageData = async () => {
    // removes everything from encrypted storage
    const success = await clearEncryptedStorage()
    if (!success) return showFailedModal(FAILED_CREDENTIALS_DELETION_MSG)

    RNRestart.Restart()

    // clear state
    resetAppState()
    resetCalendarState()
    resetBookingState()
    resetEventCreationState()
    // user profile context has different structure..
    resetProfileState()
  }

  const deleteUserAccount = async () => {
    if (!id) return
    try {
      const success = await Users.deleteUserAccount(id)
      if (!success) showFailedModal(FAILED_ACCOUNT_DELETION_MSG)

      removeStorageData()
    } catch (e) {
      showFailedModal(FAILED_ACCOUNT_DELETION_MSG)
    }
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
          "Clear all stored application data \n(includes private & public keys)."
        }>
        <SmallDangerButton
          onPressCallback={() => showCredentialsWarningModal(removeStorageData)}
          text="Clear"
        />
      </SettingsItem>
      <SettingsItem
        titleStyle={textStyle}
        title={"Delete my account and all data associated with it."}>
        <SmallDangerButton
          onPressCallback={() =>
            showAccountDeletionWarningModal(deleteUserAccount)
          }
          text="Delete"
        />
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
