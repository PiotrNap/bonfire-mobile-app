import * as React from "react";
import { View, StyleSheet, Text, Dimensions } from "react-native";

import { PencilAltIcon, RegistrationIcon } from "icons/index";
import { Typography, Colors, Sizing, Outlines } from "styles/index";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { appContext } from "contexts/contextApi";
import { PressableIcon } from "components/buttons/pressableIcon";
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view";
import { useNavigation } from "@react-navigation/native";
import { ProfileContext } from "contexts/profileContext";
import { Users } from "Api/Users";
import { OrganizerProfileDto } from "common/types/dto/organizer.dto";
import {
  getFromEncryptedStorage,
  setToEncryptedStorage,
} from "lib/encryptedStorage";
import { startChallengeSequence } from "lib/helpers";

export interface RegistrationConfirmationScreen {}

const SCREEN_WIDTH = Dimensions.get("screen").width;

export const RegistrationConfirmationScreen = () => {
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  const { profession, jobTitle, bio, timeBlockCostADA, skills, id } =
    React.useContext(ProfileContext);
  const { ref } = appContext();
  const { navigate } = useNavigation();

  const { accountType, toggleAuth } = appContext();

  const onChangePress = () => ref.current.setPage(0);
  const emptyDetails =
    !profession && !jobTitle && !bio && !timeBlockCostADA && !skills;

  const onConfirm = async () => {
    setIsLoading(true);
    try {
      const publicKey = await getFromEncryptedStorage("public");
      const secretKey = await getFromEncryptedStorage("secret");

      if (publicKey && secretKey) {
        var organizerProfileDto = new OrganizerProfileDto(
          bio,
          timeBlockCostADA ?? 0,
          profession,
          jobTitle,
          skills
        );

        const res = await Users.updateUser(organizerProfileDto, id);

        if (res.status === 201) {
          // get challenge from server
          const accessToken = await startChallengeSequence(id, true);
          await setToEncryptedStorage("accessToken", accessToken);
          navigate("Navigation Screens", {
            profileType: "organizer",
          });
        }
      } else {
        throw new Error("Occured problems while accessing keys from storage");
      }
    } catch (e) {
      console.error(e);
    }
    setIsLoading(false);
  };

  return (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={false}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={{ width: "90%" }}>
      <View style={styles.headerImage}>
        <RegistrationIcon style={styles.image} />
      </View>
      <View style={styles.header}>
        <Text style={styles.headerText}>
          Confirm details and complete registration
        </Text>
        <Text style={styles.subHeaderText}>
          You will be able to edit any personal information in your account
          profile if needed.
        </Text>
      </View>
      <View style={styles.userDetails}>
        <PressableIcon
          icon={
            <PencilAltIcon
              stroke={Colors.primary.s350}
              style={styles.userDetailsIcon}
              onPress={onChangePress}
            />
          }
          onPressCallback={onChangePress}
          styles={styles.iconWrapper}
        />
        {profession ? (
          <>
            <Text style={styles.userDetailsHeader}>Profession</Text>
            <Text style={styles.userDetailsText}>{profession}</Text>
          </>
        ) : null}
        {jobTitle ? (
          <>
            <Text style={styles.userDetailsHeader}>Job title</Text>
            <Text style={styles.userDetailsText}>{jobTitle}</Text>
          </>
        ) : null}
        {bio ? (
          <>
            <Text style={styles.userDetailsHeader}>About yourself</Text>
            <Text style={styles.userDetailsText}>{bio}</Text>
          </>
        ) : null}
        {timeBlockCostADA ? (
          <>
            <Text style={styles.userDetailsHeader}>Hourly rate</Text>
            <Text style={styles.userDetailsText}>
              {timeBlockCostADA} ₳ an hour
            </Text>
          </>
        ) : null}
        {skills ? (
          <>
            <Text style={styles.userDetailsHeader}>Skills</Text>
            <Text style={styles.userDetailsText}>{skills}</Text>
            {/*}<View style={styles.skillTags}>
              {USER_TAGS.map((tag, i) => {
                return (
                  <View key={i}>
                    <ProfileTag tag={tag} key={i} />
                  </View>
                );
              })}
             </View>*/}
          </>
        ) : null}
        {emptyDetails && (
          <View style={styles.innerTextCenter}>
            <Text style={styles.userDetailsText}>Nothing to show here...</Text>
          </View>
        )}
      </View>
      <FullWidthButton
        onPressCallback={onConfirm}
        colorScheme="dark"
        buttonType="transparent"
        loadingIndicator={isLoading}
        text="Confirm"
      />
    </KeyboardAwareScrollView>
  );
};

const styles = StyleSheet.create({
  header: {
    marginBottom: Sizing.x15,
    alignSelf: "flex-start",
  },
  scrollView: {
    alignItems: "center",
    justifyContent: "center",
  },
  headerText: {
    ...Typography.header.x65,
    color: Colors.primary.neutral,
    marginBottom: Sizing.x5,
  },
  headerImage: {
    width: SCREEN_WIDTH * 0.45,
    height: SCREEN_WIDTH * 0.45,
    marginTop: -Sizing.x20,
    marginBottom: -Sizing.x30,
    marginRight: "auto",
    marginLeft: Sizing.x10,
    alignItems: "flex-end",
  },
  image: {},
  subHeaderText: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.neutral,
  },
  innerTextCenter: {
    justifyContent: "center",
    padding: Sizing.x10,
  },
  userDetails: {
    width: "100%",
    minHeight: Sizing.x70,
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    padding: Sizing.x20,
  },
  iconWrapper: {
    zIndex: 10,
    position: "absolute",
    right: Sizing.x25,
    top: Sizing.x25,
  },
  userDetailsIcon: {
    zIndex: 9,
    width: Sizing.x30,
    height: Sizing.x30,
  },
  userDetailsHeader: {
    ...Typography.header.x10,
    color: Colors.primary.s600,
    marginTop: Sizing.x2,
    marginBottom: Sizing.x1,
  },
  userDetailsText: {
    ...Typography.body.x10,
    color: Colors.primary.s600,
  },
  skillTags: {
    flexDirection: "row",
    flexWrap: "wrap",
  },
});
