import * as React from "react";
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  Switch,
  Platform,
  ImageBackground,
} from "react-native";

import { SafeAreaView } from "react-native-safe-area-context";
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index";
import { StackScreenProps } from "@react-navigation/stack";

import { ProfileStackParamList } from "common/types/navigationTypes";
import { appContext } from "contexts/contextApi";
import {
  CogIcon,
  HearthIcon,
  LightBulbIcon,
  RightArrowIcon,
} from "icons/index";

import { NativeModal } from "components/modals/nativeModal";
import { useCameraAccess } from "lib/hooks/useCameraAccess";
import { useMediaAccess } from "lib/hooks/useMediaAccess";
import { ProfileContext } from "contexts/profileContext";
import { applyOpacity } from "../../styles/colors";
import { Users } from "Api/Users";
import { getApiUrl } from "lib/helpers";

export interface UserProfileScreenProps
  extends StackScreenProps<ProfileStackParamList, "Profile"> {}

export const UserProfileScreen = ({ navigation }: UserProfileScreenProps) => {
  const { username: _username } = React.useContext(ProfileContext);
  let username = "john";
  const { colorScheme, setColorScheme } = appContext();
  const [imagePressed, setImagePressed] = React.useState<boolean>(false);
  const [currImage, setCurrImage] = React.useState<string>("");
  const { mediaObj, launchImageLibrary } = useMediaAccess();
  const { imageObj, launchCamera } = useCameraAccess();

  React.useEffect(() => {
    /**
     *  @TODO send the user selected image to our back end
     */
    if (mediaObj) {
      //we only allow user to select one image, so take the first in array
      const { uri } = mediaObj.assets[0];

      if (uri !== currImage) {
        (async () => await Users.uploadUserImage(uri))();
        setCurrImage(uri);
      }
    }
    if (imageObj) {
      const { uri } = imageObj.assets[0];
      if (uri !== currImage) setCurrImage(uri);
    }
  }, [mediaObj, imageObj]);

  const darkMode = colorScheme === "dark";

  const setDarkMode = () => {
    setColorScheme(darkMode ? "light" : "dark");
  };

  const onImageLongPress = async () => {
    // don't run it on the browser
    if (Platform.OS !== "web") {
      setImagePressed(true);
    }
  };

  const onImagePress = () => setImagePressed(true);

  const onImagePressOut = () => setImagePressed(false);

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
            true ? (
              <ImageBackground
                source={{
                  uri: getApiUrl(
                    `/users/files/profile-image/d06c3a18-7093-44ee-8772-e7cc61ed9508`
                  ),
                }}
                onLoadEnd={() => console.log("finished loading image")}
                imageStyle={styles.profilePicImage}
                style={[
                  styles.profilePic,
                  darkMode
                    ? {
                        borderColor: Colors.primary.neutral,
                        borderWidth: Outlines.borderWidth.base,
                      }
                    : {},
                ]}>
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
              <View
                style={[
                  styles.profilePicPlaceholder,
                  {
                    backgroundColor: Colors.neutral.s300,
                  },
                  darkMode && {
                    borderColor: Colors.primary.neutral,
                    borderWidth: Outlines.borderWidth.base,
                  },
                ]}>
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
                  <Text style={styles.profilePicLetter}>{username[0]}</Text>
                  <View style={styles.profilePicEdit}>
                    <Text style={styles.profilePicEditText}>Edit</Text>
                  </View>
                </Pressable>
              </View>
            )
          }></NativeModal>
        <Text
          style={[
            colorScheme == "light"
              ? styles.headerText_ligth
              : styles.headerText_dark,
          ]}>
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
        <Pressable style={styles.navigationItem} onPress={() => {}}>
          <HearthIcon
            width={26}
            height={26}
            color={
              colorScheme === "light"
                ? Colors.primary.brand
                : Colors.primary.neutral
            }
            strokeWidth={1.6}
          />
          <Text
            style={
              colorScheme === "light"
                ? styles.navigationItemText_light
                : styles.navigationItemText_dark
            }>
            Favorites
          </Text>
          <RightArrowIcon
            width={26}
            height={26}
            color={
              colorScheme === "light"
                ? Colors.primary.brand
                : Colors.primary.neutral
            }
            style={styles.navigationItemRightIcon}
            strokeWidth={1.6}
          />
        </Pressable>
        <Pressable style={styles.navigationItem} onPress={() => {}}>
          <CogIcon
            width={26}
            height={26}
            color={
              colorScheme === "light"
                ? Colors.primary.brand
                : Colors.primary.neutral
            }
            strokeWidth={1.6}
          />
          <Text
            style={
              colorScheme === "light"
                ? styles.navigationItemText_light
                : styles.navigationItemText_dark
            }>
            Settings
          </Text>
          <RightArrowIcon
            width={26}
            height={26}
            color={
              colorScheme === "light"
                ? Colors.primary.brand
                : Colors.primary.neutral
            }
            style={styles.navigationItemRightIcon}
            strokeWidth={1.6}
          />
        </Pressable>
        <View style={[styles.navigationItem, { marginTop: "auto" }]}>
          <LightBulbIcon
            width={26}
            height={26}
            color={
              colorScheme === "light"
                ? Colors.primary.brand
                : Colors.primary.neutral
            }
            strokeWidth={1.6}
          />
          <Text
            style={
              colorScheme === "light"
                ? styles.navigationItemText_light
                : styles.navigationItemText_dark
            }>
            Dark mode
          </Text>
          <Switch
            trackColor={{
              false: Colors.primary.brand,
              true: Colors.primary.neutral,
            }}
            thumbColor={darkMode ? Colors.primary.s600 : Colors.primary.neutral}
            ios_backgroundColor={Colors.primary.brand}
            onValueChange={setDarkMode}
            value={darkMode}
            style={{ marginLeft: "auto" }}
          />
        </View>
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea_light: {
    flex: 1,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeaArea_dark: {
    flex: 1,
    backgroundColor: Colors.primary.s600,
    alignItems: "center",
  },
  headerNavigation: {
    alignItems: "center",
    marginVertical: Sizing.x30,
  },
  headerText_ligth: {
    ...Typography.header.x45,
    color: Colors.primary.s600,
    marginVertical: Sizing.x8,
  },
  headerText_dark: {
    ...Typography.header.x45,
    color: Colors.primary.neutral,
    marginVertical: Sizing.x8,
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
    width: Sizing.x85,
    height: Sizing.x85,
    overflow: "hidden",
  },
  profilePicPlaceholder: {
    borderRadius: Outlines.borderRadius.max,
    width: Sizing.x85,
    height: Sizing.x85,
    overflow: "hidden",
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
  navigationItem: {
    flexDirection: "row",
    alignItems: "center",
    width: "85%",
    marginBottom: Sizing.x20,
  },
  navigationItemRightIcon: {
    marginLeft: "auto",
  },
  navigationItemText_light: {
    ...Typography.subHeader.x35,
    marginLeft: Sizing.x10,
    color: Colors.primary.s600,
  },
  navigationItemText_dark: {
    ...Typography.subHeader.x35,
    marginLeft: Sizing.x10,
    color: Colors.primary.neutral,
  },
});
