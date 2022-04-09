import * as React from "react"
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  Switch,
  Platform,
  ImageBackground,
} from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index"
import { StackScreenProps } from "@react-navigation/stack"
import Avatar from "react-native-boring-avatars"

import { ProfileStackParamList } from "common/types/navigationTypes"
import { appContext } from "contexts/contextApi"
import { CogIcon, HearthIcon, LightBulbIcon } from "icons/index"

import { NativeModal } from "components/modals/nativeModal"
import { useCameraAccess } from "lib/hooks/useCameraAccess"
import { useMediaAccess } from "lib/hooks/useMediaAccess"
import { ProfileContext } from "contexts/profileContext"
import { applyOpacity } from "../../styles/colors"
import { Users } from "Api/Users"
import { getApiUrl } from "lib/helpers"
import { SettingsNavigationItem } from "components/profile/settingsNavigationItem"
import { SettingsItem } from "components/profile/settingsItem"
import { useUserInfo } from "lib/hooks/useUserInfo"

export interface UserProfileProps
  extends StackScreenProps<ProfileStackParamList, "Profile"> {}

export const UserProfile = ({ navigation }: UserProfileProps) => {
  const { username, id } = React.useContext(ProfileContext)
  const { userInfo } = useUserInfo(id)
  const { colorScheme, setColorScheme } = appContext()
  const [imagePressed, setImagePressed] = React.useState<boolean>(false)
  const [currImage, setCurrImage] = React.useState<string>("")
  const { mediaObj, launchImageLibrary } = useMediaAccess()
  const { imageObj, launchCamera } = useCameraAccess()

  console.log("user info", userInfo)

  React.useEffect(() => {
    /**
     *  @TODO send the user selected image to our back end
     */
    if (mediaObj) {
      //we only allow user to select one image, so take the first in array
      const { uri } = mediaObj.assets[0]

      if (uri !== currImage) {
        ;(async () => await Users.uploadUserImage(uri))()
        setCurrImage(uri)
      }
    }
    if (imageObj) {
      const { uri } = imageObj.assets[0]
      if (uri !== currImage) setCurrImage(uri)
    }
  }, [mediaObj, imageObj])

  const darkMode = colorScheme === "dark"

  const setDarkMode = () => {
    setColorScheme(darkMode ? "light" : "dark")
  }

  const onImageLongPress = async () => {
    // don't run it on the browser
    if (Platform.OS !== "web") {
      setImagePressed(true)
    }
  }

  const onImagePress = () => setImagePressed(true)

  const onImagePressOut = () => setImagePressed(false)

  return (
    <SafeAreaView
      style={[
        colorScheme == "light" ? styles.safeArea_light : styles.safeaArea_dark,
      ]}>
      <View style={styles.headerNavigation}>
        <NativeModal
          cameraAccessCb={launchCamera}
          mediaLibraryCb={launchImageLibrary}
          child={
            currImage ? (
              <ImageBackground
                source={{
                  uri: getApiUrl(`/users/files/profile-image/${id}`),
                }}
                onLoadEnd={() => console.log("finished loading image")}
                imageStyle={styles.profilePicImage}
                style={styles.profilePic}>
                <Pressable
                  onPressIn={onImagePress}
                  onPressOut={onImagePressOut}
                  onLongPress={onImageLongPress}
                  hitSlop={5}
                  pressRetentionOffset={5}
                  style={[
                    styles.profilePicWrapper,
                    imagePressed ? { backgroundColor: "rgba(0,0,0,.15)" } : {},
                  ]}>
                  <View style={styles.profilePicEdit}>
                    <Text style={styles.profilePicEditText}>Edit</Text>
                  </View>
                </Pressable>
              </ImageBackground>
            ) : (
              <View style={styles.profilePicPlaceholder}>
                <Pressable
                  onPressIn={onImagePress}
                  onPressOut={onImagePressOut}
                  onLongPress={onImageLongPress}
                  hitSlop={5}
                  pressRetentionOffset={5}
                  style={styles.profilePicWrapper}>
                  <Avatar
                    size={"100%"}
                    name={username}
                    variant="beam"
                    colors={[
                      Colors.primary.s600,
                      Colors.primary.s200,
                      Colors.neutral.s100,
                    ]}
                  />
                  <View style={[styles.profilePicEdit, {}]}>
                    <Text style={styles.profilePicEditText}>Edit</Text>
                  </View>
                </Pressable>
              </View>
            )
          }></NativeModal>
        <Text
          style={[
            styles.headerText,
            colorScheme == "light"
              ? styles.headerText_ligth
              : styles.headerText_dark,
          ]}
          numberOfLines={1}
          ellipsizeMode="tail">
          {username}
        </Text>
        <Pressable
          style={Buttons.applyOpacity(
            colorScheme === "light" ? styles.button_light : styles.button_dark
          )}
          onPress={() => navigation.navigate("Edit Profile")}>
          <Text
            style={
              colorScheme === "light"
                ? styles.buttonText_light
                : styles.buttonText_dark
            }>
            Edit Profile
          </Text>
        </Pressable>
      </View>
      <View style={styles.mainNavigation}>
        <SettingsNavigationItem
          icon={HearthIcon}
          onPressCallback={() => {}}
          title="Favorites"
        />
        <SettingsNavigationItem
          icon={CogIcon}
          onPressCallback={() => navigation.navigate("Profile Settings")}
          title="Settings"
        />
        <SettingsItem
          icon={LightBulbIcon}
          title="Dark Mode"
          customStyle={{ marginTop: "auto" }}>
          <Switch
            trackColor={{
              false: Colors.neutral.s400,
              true: Colors.primary.brand,
            }}
            thumbColor={Colors.primary.neutral}
            ios_backgroundColor={Colors.neutral.s400}
            onValueChange={setDarkMode}
            value={darkMode}
            style={{ marginLeft: "auto" }}
          />
        </SettingsItem>
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeaArea_dark: {
    flex: 1,
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
  },
  headerNavigation: {
    alignItems: "center",
    marginVertical: Sizing.x30,
  },
  headerText: {
    ...Typography.header.x45,
    marginVertical: Sizing.x8,
    maxWidth: "60%",
  },
  headerText_ligth: {
    color: Colors.primary.s600,
  },
  headerText_dark: {
    color: Colors.primary.neutral,
  },
  mainNavigation: {
    flex: 1,
    alignItems: "center",
    marginTop: Sizing.x10,
    width: "100%",
  },
  button_light: {
    ...Buttons.bar.secondary,
    padding: Sizing.x8,
    backgroundColor: Colors.primary.s800,
    ...Outlines.shadow.lifted_noElevation,
  },
  button_dark: {
    ...Buttons.bar.secondary,
    padding: Sizing.x8,
    backgroundColor: Colors.primary.neutral,
    ...Outlines.shadow.lifted_noElevation,
  },
  buttonText_light: {
    ...Typography.header.x30,
    textAlignVertical: "center",
    includeFontPadding: false,
    paddingHorizontal: Sizing.x2,
    textAlign: "center",
    color: Colors.primary.neutral,
  },
  buttonText_dark: {
    ...Typography.header.x30,
    textAlignVertical: "center",
    includeFontPadding: false,
    paddingHorizontal: Sizing.x2,
    textAlign: "center",
    color: Colors.primary.s600,
  },
  profilePicWrapper: {
    flex: 1,
    justifyContent: "center",
    borderRadius: 999,
    ...Outlines.shadow.lifted_noElevation,
  },
  profilePicImage: {
    borderRadius: 999,
    resizeMode: "cover",
  },
  profilePic: {
    borderRadius: Outlines.borderRadius.max,
    borderColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.base,
    width: Sizing.x85,
    height: Sizing.x85,
    overflow: "hidden",
  },
  profilePicPlaceholder: {
    borderRadius: Outlines.borderRadius.max,
    width: Sizing.x85,
    height: Sizing.x85,
    overflow: "hidden",
    backgroundColor: Colors.neutral.s300,
    borderColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.base,
  },
  profilePicLetter: {
    alignSelf: "center",
    color: "white",
    ...Typography.roboto.medium,
    fontSize: Sizing.x60,
  },
  profilePicEdit: {
    backgroundColor: applyOpacity(Colors.neutral.s500, 0.6),
    width: "100%",
    height: "25%",
    position: "absolute",
    bottom: 0,
  },
  profilePicEditText: {
    color: "white",
    textAlign: "center",
    ...Typography.subHeader.x20,
  },
})
