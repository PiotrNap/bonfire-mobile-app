import * as React from "react"
import { Pressable, StyleSheet, Text, View } from "react-native"
import { StackScreenProps } from "@react-navigation/stack"
import RNRestart from "react-native-restart"

import { LeftArrowIcon } from "assets/icons"
import { Layout } from "components/layouts/basicLayout"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import {
  appContext,
  bookingContext,
  eventCreationContext,
  myCalendarContext,
} from "contexts/contextApi"
import { ProfileStackParamList } from "common/types/navigationTypes"
import { SettingsItem } from "components/profile/settingsItem"
import { SmallDangerButton } from "components/buttons/smallDangerButton"
import { clearEncryptedStorage, removeFromEncryptedStorage } from "lib/encryptedStorage"
import { ProfileContext } from "contexts/profileContext"
import {
  showAccountDeletionWarningModal,
  showCredentialsLossWarningModal,
  showStandardModal,
} from "lib/modalAlertsHelpers"
import { FAILED_ACCOUNT_DELETION_MSG } from "lib/errors"
import { Users } from "Api/Users"
import { CustomSwitch } from "components/rnWrappers/customSwitch"
import { UserSettings } from "common/interfaces/appInterface"
import { BodyText } from "components/rnWrappers/bodyText"
import { GIT_HASH } from "../../gitHash"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { SmallButton } from "components/buttons/smallButton"
import { deleteKeysFromStorage } from "lib/wallet/storage"
import { showErrorToast, showSuccessToast } from "lib/helpers"
import AsyncStorage from "@react-native-async-storage/async-storage"
import { Authenticator } from "components/modals/Authenticator"
import { COLLATERAL_STORAGE_KEY } from "lib/wallet/utils"

type ScreenProps = StackScreenProps<ProfileStackParamList, "Profile Settings">

export const UserProfileSettings = ({ navigation }: ScreenProps) => {
  const { id, collateralUtxoId, setCollateralUtxoId } = React.useContext(ProfileContext)
  const { colorScheme, resetAppState, userSettings, setUserSettings } = appContext()
  const { resetCalendarState } = myCalendarContext()
  const { resetBookingState } = bookingContext()
  const { resetEventCreationState } = eventCreationContext()
  const { resetProfileState } = React.useContext(ProfileContext)
  const [isOfflineMnemonic, setIsOfflineMnemonic] = React.useState<boolean>(false)
  const [authenticatorVisible, setAuthenticatorVisible] = React.useState<boolean>(false)

  React.useEffect(() => {
    ;(async () => {
      let res = await AsyncStorage.getItem("isOfflineMnemonic")
      setIsOfflineMnemonic(Boolean(res))
    })()
  }, [])

  const isLightMode = colorScheme === "light"
  const { color: _, ...textStyle } = Typography.subHeader.x10
  const switchTextStyle = {
    color: isLightMode ? Colors.primary.s600 : Colors.primary.neutral,
    ...Typography.subHeader.x10,
  }

  const onBackNavigationPress = () => navigation.goBack()
  const removeStorageData = async () => {
    try {
      // removes every credential from encrypted storage
      await clearEncryptedStorage()
      // removes every secret key associated with users wallet
      await deleteKeysFromStorage()

      // clear local state
      resetAppState()
      resetCalendarState()
      resetBookingState()
      resetEventCreationState()
      resetProfileState()

      RNRestart.Restart() // restarts the app
    } catch (e) {
      showErrorToast(e)
    }
  }
  const deleteUserAccount = async () => {
    if (!id) return
    try {
      const success = await Users.deleteUserAccount(id)
      if (!success) showStandardModal(FAILED_ACCOUNT_DELETION_MSG)

      await removeStorageData()
      showStandardModal(
        `We're sorry to see you go! Please let us know what can we improve ${process.env.BUSINESS_EMAIL}. Bonfire will keep things warm for you.`,
        "Account removed"
      )
    } catch (e) {
      showStandardModal(FAILED_ACCOUNT_DELETION_MSG)
    }
  }
  // const onShowPastCalendarEvents = async () => {
  //   const newSettings: UserSettings = {
  //     ...userSettings,
  //     showPastCalendarEvents: !userSettings?.showPastCalendarEvents,
  //   }
  //   setUserSettings(newSettings)
  //   try {
  //     await removeFromEncryptedStorage("user-settings")
  //     await Users._("put", `users/${id}/settings`, newSettings)
  //   } catch (e) {
  //     showStandardModal("We weren't able to update your preferences in our Database.")
  //   }
  // }
  const onPreviewMnemonicPress = () => {
    setAuthenticatorVisible(true)
    // show modal with password/biometric authentication
  }
  const onUnlockCollataralPress = async () => {
    try {
      await AsyncStorage.removeItem(COLLATERAL_STORAGE_KEY)
      showSuccessToast("Success", "Your collateral UTxO has been unlocked.")
      setCollateralUtxoId("")
    } catch (e) {
      showErrorToast(
        "We couldn't unlock your collateral for some reason.",
        "Something went wrong..."
      )
    }
  }
  const onAuthenticated = (mnemonic: string | void) => {
    setAuthenticatorVisible(false)
    if (!mnemonic)
      return showErrorToast(
        "Something went wrong. Have you enabled this option during registration?"
      )
    showStandardModal(mnemonic, "Remember to keep it secure and not share with anyone.")
    mnemonic = ""
  }
  const onHideAuthenticator = () => setAuthenticatorVisible(false)

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
      {collateralUtxoId && (
        <SettingsItem titleStyle={textStyle} title={"Unlock collateral UTxO"}>
          <SmallButton
            onPress={onUnlockCollataralPress}
            title="Unlock"
            customStyle={{ width: "100%", justifyContent: "center" }}
          />
        </SettingsItem>
      )}
      {/*
      @TODO after beta release
      <SettingsItem titleStyle={textStyle} title={"Show past events on my calendar."}>
        <Text style={switchTextStyle}>
          {userSettings?.showPastCalendarEvents ? "Enabled" : "Disabled"}
        </Text>
        <CustomSwitch
          onValueChange={onShowPastCalendarEvents}
          value={userSettings?.showPastCalendarEvents ? true : false}
        />
      </SettingsItem>
      */}
      <View
        style={[
          styles.sensitiveInfoSection,
          {
            borderColor:
              colorScheme === "light" ? Colors.primary.s600 : Colors.primary.neutral,
          },
        ]}>
        <View style={styles.sectionHeader}>
          <SubHeaderText
            customStyle={Typography.roboto.bold}
            colors={[Colors.danger.s400]}>
            Danger Zone
          </SubHeaderText>
        </View>
        {isOfflineMnemonic && (
          <SettingsItem
            titleStyle={textStyle}
            title={"Preview my mnemonic phrase (requires authentication)"}>
            <SmallButton
              onPress={onPreviewMnemonicPress}
              title="Preview"
              customStyle={{ width: "100%", justifyContent: "center" }}
            />
          </SettingsItem>
        )}
        <SettingsItem
          titleStyle={textStyle}
          title={
            "Remove all data stored on this device \n(includes private & public keys)"
          }>
          <SmallDangerButton
            onPressCallback={() => showCredentialsLossWarningModal(removeStorageData)}
            text="Remove"
          />
        </SettingsItem>
        <SettingsItem titleStyle={textStyle} title={"Deactivate my account"}>
          <SmallDangerButton
            onPressCallback={() => showAccountDeletionWarningModal(deleteUserAccount)}
            text="Deactivate"
          />
        </SettingsItem>
      </View>
      <BodyText
        customStyle={{
          ...Typography.roboto.regular,
          fontSize: 15,
        }}>
        v0.1.0 {String(GIT_HASH).substring(0, 8)}
      </BodyText>
      {authenticatorVisible && (
        <Authenticator
          authRequestType="mnemonic"
          showAuthenticator={authenticatorVisible}
          onAuthenticatedCb={onAuthenticated}
          onHideAuthenticatorCb={onHideAuthenticator}
        />
      )}
    </Layout>
  )
}

const styles = StyleSheet.create({
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
  sectionHeader: {
    alignSelf: "center",
    marginHorizontal: "auto",
    marginVertical: Sizing.x5,
  },
  sensitiveInfoSection: {
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    padding: Sizing.x8,
    marginVertical: Sizing.x5,
  },
})
