import * as React from "react"
import { View, Text, StyleSheet, Pressable, Platform } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index"
import { StackScreenProps } from "@react-navigation/stack"
import ContentLoader, { Circle } from "react-content-loader/native"
import Avatar from "react-native-boring-avatars"

import { ProfileStackParamList } from "common/types/navigationTypes"
import { appContext } from "contexts/contextApi"
import { CogIcon, ErrorIcon, HearthIcon, LightBulbIcon } from "icons/index"

import { NativeModal } from "components/modals/nativeModal"
import { useCameraAccess } from "lib/hooks/useCameraAccess"
import { useMediaAccess } from "lib/hooks/useMediaAccess"
import { ProfileContext } from "contexts/profileContext"
import { applyOpacity } from "../../styles/colors"
import { Users } from "Api/Users"
import { SettingsNavigationItem } from "components/profile/settingsNavigationItem"
import { SettingsItem } from "components/profile/settingsItem"
import { useUserInfo } from "lib/hooks/useUserInfo"
import FastImage from "react-native-fast-image"
import { CustomSwitch } from "components/rnWrappers/customSwitch"
import { showNSFWImageModal } from "lib/modalAlertsHelpers"
import { SlideTopModal } from "components/modals/slideTopModal"

export interface UserProfileProps
  extends StackScreenProps<ProfileStackParamList, "Profile"> {}

export const UserProfile = ({ navigation }: UserProfileProps) => {
  const { isLoading } = useUserInfo() // fetch new user informations
  const { getUserProfile, setImageBase64 } = React.useContext(ProfileContext)
  const { colorScheme, setColorScheme } = appContext()
  const { mediaObj, setMediaObj, launchImageLibrary } = useMediaAccess()
  const { imageObj, setImgObj, launchCamera } = useCameraAccess()
  const [imagePressed, setImagePressed] = React.useState<boolean>(false)
  const [errorMessage, setErrorMessage] = React.useState<string>("")
  const [isModalVisible, setIsModalVisible] = React.useState<boolean>(false)

  const userInfo = getUserProfile()
  const [currImage, setCurrImage] = React.useState<any>(userInfo.imageBase64)

  React.useEffect(() => {
    const obj = mediaObj?.assets[0] || imageObj?.assets[0]
    if (!obj && userInfo.imageBase64) return setCurrImage(userInfo.imageBase64)

    if (obj)
      (async () => {
        const { uri, base64 } = obj

        try {
          await Users.uploadUserImage(uri)
          setImageBase64(base64)
        } catch (e) {
          if (e.response.status === 422) return showNSFWImageModal()
          setErrorMessage("Something went wrong while updating profile image.")
        }
      })()
    setMediaObj(null)
    setImgObj(null)
  }, [mediaObj?.assets[0].uri, imageObj?.assets[0].uri, userInfo])

  const darkMode = colorScheme === "dark"
  const ModalIcon = (
    <ErrorIcon
      stroke={Colors.primary.neutral}
      width={Sizing.x60}
      height={Sizing.x60}
      strokeWidth={1.5}
    />
  )

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
  const updateCurrImage = () => setCurrImage("")
  const modalHideCallback = () => setIsModalVisible(false)

  return (
    <>
      <SafeAreaView
        style={[
          colorScheme == "light"
            ? styles.safeArea_light
            : styles.safeaArea_dark,
        ]}>
        <View style={styles.headerNavigation}>
          <NativeModal
            cameraAccessCb={launchCamera}
            mediaLibraryCb={launchImageLibrary}
            onImageDeleted={updateCurrImage}
            child={
              currImage ? (
                //@ts-ignore
                <FastImage
                  source={{
                    uri: `data:image/png;base64,${userInfo.imageBase64}`,
                  }}
                  style={styles.profilePic}>
                  <Pressable
                    onPressIn={onImagePress}
                    onPressOut={onImagePressOut}
                    onLongPress={onImageLongPress}
                    hitSlop={5}
                    pressRetentionOffset={5}
                    style={[
                      styles.profilePicWrapper,
                      imagePressed
                        ? { backgroundColor: "rgba(0,0,0,.15)" }
                        : {},
                    ]}>
                    <View style={styles.profilePicEdit}>
                      <Text style={styles.profilePicEditText}>Edit</Text>
                    </View>
                  </Pressable>
                </FastImage>
              ) : (
                <View style={styles.profilePicPlaceholder}>
                  <Pressable
                    onPressIn={onImagePress}
                    onPressOut={onImagePressOut}
                    onLongPress={onImageLongPress}
                    hitSlop={5}
                    pressRetentionOffset={5}
                    style={styles.profilePicWrapper}>
                    {!isLoading ? (
                      <Avatar
                        size={"100%"}
                        name={userInfo.username}
                        variant="beam"
                        colors={[
                          Colors.primary.s600,
                          Colors.primary.s200,
                          Colors.neutral.s100,
                        ]}
                      />
                    ) : (
                      <ContentLoader
                        speed={0.5}
                        width={Sizing.x85}
                        height={Sizing.x85}
                        viewBox={`0 0 ${Sizing.x85} ${Sizing.x85}`}
                        foregroundColor={Colors.neutral.s150}
                        backgroundColor={Colors.neutral.s100}>
                        {/*@ts-ignore*/}
                        <Circle cx="50%" cy="50%" r={Sizing.x85 / 2} />
                      </ContentLoader>
                    )}
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
            {userInfo.username}
          </Text>
          <Pressable
            style={Buttons.applyOpacity(
              colorScheme === "light" ? styles.button_light : styles.button_dark
            )}
            onPress={() =>
              navigation.navigate<"Edit Profile">({
                name: "Edit Profile",
                params: { userInfo },
              })
            }>
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
            <CustomSwitch
              onValueChange={setDarkMode}
              value={darkMode}
              style={{ marginLeft: "auto" }}
            />
          </SettingsItem>
        </View>
      </SafeAreaView>
      {isModalVisible && (
        <SlideTopModal
          icon={ModalIcon}
          isModalVisible={isModalVisible}
          modalContent={errorMessage}
          backgroundColor={Colors.danger.s300}
          hideCallback={modalHideCallback}
        />
      )}
    </>
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
  profilePic: {
    backgroundColor: Colors.neutral.s200,
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
