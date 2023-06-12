import * as React from "react"
import { View, StyleSheet, Text, Dimensions } from "react-native"

import { PencilAltIcon, RegistrationIcon } from "icons/index"
import { Typography, Colors, Sizing, Outlines } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { appContext } from "contexts/contextApi"
import { PressableIcon } from "components/buttons/pressableIcon"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { useNavigation } from "@react-navigation/native"
import { ProfileContext } from "contexts/profileContext"
import { Users } from "Api/Users"
import { OrganizerProfileDto } from "common/types/dto/organizer.dto"
import {
  getFromEncryptedStorage,
  setToEncryptedStorage,
} from "lib/encryptedStorage"
import { startChallengeSequence } from "lib/helpers"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"

export interface RegistrationConfirmationScreen {}

const SCREEN_WIDTH = Dimensions.get("screen").width

export const RegistrationConfirmationScreen = () => {
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const { profession, jobTitle, bio, hourlyRate, skills, id } =
    React.useContext(ProfileContext)
  const { ref } = appContext()
  const { navigate } = useNavigation()

  const onChangePress = () => ref.current.setPage(0)
  const emptyDetails =
    !profession && !jobTitle && !bio && !hourlyRate && !skills

  const onConfirm = async () => {
    setIsLoading(true)
    try {
      const publicKey = await getFromEncryptedStorage("pubKey")
      const secretKey = await getFromEncryptedStorage("privKey")

      if (publicKey && secretKey) {
        var organizerProfileDto = new OrganizerProfileDto(
          bio,
          hourlyRate ?? { ada: 0, gimbals: 0 },
          profession,
          jobTitle,
          skills
        )

        const res = await Users.updateUser(organizerProfileDto, id)

        if (res.status === 201) {
          const authResponseDTO = await startChallengeSequence(
            publicKey,
            id,
            true
          )
          await setToEncryptedStorage("auth-credentials", authResponseDTO)
          navigate("Navigation Screens")
        }
      } else {
        throw new Error("Occured problems while accessing keys from storage")
      }
    } catch (e) {
      console.error(e)
    }
    setIsLoading(false)
  }

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
        <HeaderText>Confirm details and complete registration</HeaderText>
        <SubHeaderText customColorScheme="dark">
          You will be able to edit any personal information in your account
          profile if needed.
        </SubHeaderText>
      </View>
      <View style={styles.userDetails}>
        <PressableIcon
          icon={
            <PencilAltIcon
              stroke={Colors.primary.s600}
              style={styles.userDetailsIcon}
              strokeWidth="1.5"
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
            <Text style={styles.userDetailsHeader}>Job Title</Text>
            <Text style={styles.userDetailsText}>{jobTitle}</Text>
          </>
        ) : null}
        {bio ? (
          <>
            <Text style={styles.userDetailsHeader}>About Yourself</Text>
            <Text style={styles.userDetailsText}>{bio}</Text>
          </>
        ) : null}
        {hourlyRate?.gimbals || hourlyRate?.ada ? (
          <>
            <Text style={styles.userDetailsHeader}>Hourly Rate (ADA)</Text>
            <Text style={styles.userDetailsText}>{hourlyRate.ada} an hour</Text>
            <Text style={styles.userDetailsHeader}>Hourly Rate (Gimbals)</Text>
            <Text style={styles.userDetailsText}>
              {hourlyRate.gimbals} an hour
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
        isOnboarding
      />
    </KeyboardAwareScrollView>
  )
}

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
})
