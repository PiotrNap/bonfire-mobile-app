import * as React from "react"
import { Alert, Platform, Pressable, StyleSheet, View } from "react-native"
import { LeftArrowIcon } from "assets/icons"
import { Layout } from "screens/layouts/basicLayout"

import { Colors, Sizing, Typography } from "styles/index"
import { appContext } from "contexts/contextApi"
import { StackScreenProps } from "@react-navigation/stack"
import { ProfileStackParamList } from "common/types/navigationTypes"
import { SettingsItem } from "components/profile/settingsItem"
import { SmallDangerButton } from "components/buttons/smallDangerButton"
import { removeFromEncryptedStorage } from "lib/encryptedStorage"

type ScreenProps = StackScreenProps<ProfileStackParamList, "Profile Settings">

export const UserProfileSettings = ({ navigation }: ScreenProps) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const isAndroid = Platform.OS === "android"

  const onBackNavigationPress = () => navigation.goBack()
  const removeStorageData = async () => {
    // deletes: auth-credentials, privKey, pubKey, mnemonics(?)
    await removeFromEncryptedStorage("auth-credentials")
    await removeFromEncryptedStorage("privKey")
    await removeFromEncryptedStorage("pubKey")
    // TODO uncomment this once wallet is implemented
    // await removeFromEncryptedStorage("seedPhrase")
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
              "None of your credentials were removed. You can continue using the application."
            ),
        },
      ],
      isAndroid ? { cancelable: true } : {}
    )
  }

  const { color: _, ...textStyle } = Typography.subHeader.x20

  return (
    <Layout>
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
          "Remove all application data \n(includes private & public keys)"
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
