import * as React from "react"
import { View, StyleSheet, Text, Dimensions, Pressable, Linking } from "react-native"

import { RegistrationIcon } from "icons/index"
import { Typography, Colors, Sizing, Outlines, Forms } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { walletContext } from "contexts/contextApi"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { useNavigation } from "@react-navigation/native"
import { ProfileContext } from "contexts/profileContext"
import { Users } from "Api/Users"
import { setToEncryptedStorage } from "lib/encryptedStorage"
import { showErrorToast, startChallengeSequence } from "lib/helpers"
import { HeaderText } from "components/rnWrappers/headerText"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { generateKeyPair } from "lib/tweetnacl"
import { setAuthorizationToken } from "Api/base"
import { TextInput } from "react-native-gesture-handler"
import { formStyleDark } from "../../styles/forms"
import { Checkbox } from "components/forms/Checkbox"
import { BodyText } from "components/rnWrappers/bodyText"
import { ModalState, SlideDownModal } from "components/modals/SlideDownModal"
import { InAppBrowser } from "@stytch/react-native-inappbrowser-reborn"

export interface RegistrationConfirmationScreen {}

const SCREEN_WIDTH = Dimensions.get("screen").width
const WEBSITE_URL = process.env.WEBSITE_URL || ""

export const RegistrationConfirmationScreen = ({ pagerRef }: any) => {
  const [modalState, setModalState] = React.useState<ModalState>({
    visible: false,
    type: null,
  })
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [isPreviewingTermsOfService, setIsPreviewingTermsOfService] =
    React.useState<boolean>(false)
  const [isPreviewingPrivacyPolicy, setIsPreviewingPrivacyPolicy] =
    React.useState<boolean>(false)
  React.useState<boolean>(false)
  const [isBetaTestingActive, setIsBetaTestingActive] = React.useState<boolean>(false)
  const [acceptedTerms, setAcceptedTerms] = React.useState<boolean>(false)
  const [betaTesterCode, setBetaTesterCode] = React.useState<string>("")
  const [linkingAvailable, setLinkingAvailable] = React.useState<boolean>(false)
  const { baseAddress, resetSecrets, accountPubKeyHex } = walletContext()
  const {
    setID,
    setDeviceID,
    username,
    hourlyRateAda,
    bio,
    jobTitle,
    skills,
    profession,
  } = React.useContext(ProfileContext)
  const navigation = useNavigation()

  React.useEffect(() => {
    ;(async () => {
      try {
        let isActive = await Users.getIsBetaTestingActive()
        setIsBetaTestingActive(!!isActive)
        const supportsLinking = await Linking.canOpenURL(WEBSITE_URL)
        if (supportsLinking) setLinkingAvailable(supportsLinking)
      } catch (e) {}
    })()
  }, [])

  const showTermsOfService = async () => {
    try {
      const url = WEBSITE_URL + "/terms-of-service"
      if (await InAppBrowser.isAvailable()) {
        const result = await InAppBrowser.open(url, {
          //@TODO do I need all of those props??
          // iOS Properties
          dismissButtonStyle: "cancel",
          preferredBarTintColor: Colors.primary.s600,
          preferredControlTintColor: "white",
          readerMode: false,
          animated: true,
          modalPresentationStyle: "fullScreen",
          modalTransitionStyle: "coverVertical",
          modalEnabled: true,
          enableBarCollapsing: false,
          // Android Properties
          showTitle: true,
          toolbarColor: Colors.primary.s600,
          secondaryToolbarColor: "black",
          navigationBarColor: "black",
          navigationBarDividerColor: "white",
          enableUrlBarHiding: true,
          enableDefaultShare: true,
          forceCloseOnRedirection: false,
          // Specify full animation resource identifier(package:anim/name)
          // or only resource name(in case of animation bundled with app).
          animations: {
            startEnter: "slide_in_right",
            startExit: "slide_out_left",
            endEnter: "slide_in_left",
            endExit: "slide_out_right",
          },
          headers: {
            "my-custom-header": "my custom header value",
          },
        })
      } else {
        navigation.navigate("Legal Document", { type: "terms-of-service" })
      }
    } catch (e) {
      console.error(e)
    }
  }

  const showPrivacyPolicy = () => {
    if (linkingAvailable) {
      setIsPreviewingPrivacyPolicy(true)
    } else {
      navigation.navigate("Legal Document", { type: "privacy-policy" })
    }
  }
  const onAcceptedTerms = () => setAcceptedTerms((p) => !p)
  const onConfirm = async () => {
    setIsLoading(true)
    try {
      const keyPair = generateKeyPair()
      let secretKey = Buffer.from(keyPair?.secretKey).toString("base64")
      let publicKey = Buffer.from(keyPair?.publicKey).toString("base64")

      // create new user record
      const newUserDTO = await Users.createAccount({
        username,
        hourlyRateAda,
        bio,
        jobTitle,
        skills,
        profession,
        publicKey,
        baseAddress,
        walletPublicKey: accountPubKeyHex,
      })

      if (!newUserDTO || !newUserDTO.id)
        throw new Error(
          `Something went wrong during registration. Please reload the app and try again`
        )

      // register for beta testing
      if (betaTesterCode) {
        await Users.registerForBetaTesting(betaTesterCode, baseAddress)
      }

      const authResponseDTO = await startChallengeSequence(
        secretKey,
        newUserDTO.deviceID,
        newUserDTO.id
      )

      console.log(`
        adding to encrypted storage:
            deviceID: ${newUserDTO.deviceID}
            authCred: ${authResponseDTO}
            devicePubKey: ${publicKey}
            deviceSecKey: ${secretKey}
            `)
      await setToEncryptedStorage("device-id", newUserDTO.deviceID)
      await setToEncryptedStorage("auth-credentials", authResponseDTO)
      await setToEncryptedStorage("device-privKey", secretKey)
      await setToEncryptedStorage("device-pubKey", publicKey)

      setDeviceID(newUserDTO.deviceID)
      setID(newUserDTO.id)
      if (authResponseDTO?.accessToken) setAuthorizationToken(authResponseDTO.accessToken)

      // clean up secret keys
      pagerRef.current.state = null
      resetSecrets()
      secretKey = ""
      publicKey = ""

      //@ts-ignore
      navigation.navigate("Navigation Screens")
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
    }
  }

  const TextComponent = React.useCallback(
    ({ children, isLink }) => (
      <BodyText
        customStyle={{
          fontFamily: isLink ? "Roboto-Black" : "Roboto-Regular",
          fontSize: Sizing.x14,
          textDecorationLine: isLink ? "underline" : "none",
        }}
        changingColorScheme
        customColorScheme="dark"
        colors={[Colors.primary.s800, Colors.primary.neutral]}>
        {children}
      </BodyText>
    ),
    []
  )

  return (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={true}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={{ width: "90%" }}>
      <View style={styles.headerImage}>
        <RegistrationIcon style={styles.image} />
      </View>
      <View style={styles.header}>
        <HeaderText>Complete registration</HeaderText>
        <SubHeaderText customColorScheme="dark">
          You will be able to edit any personal information in your account profile
          settings if needed.
        </SubHeaderText>
      </View>
      <View style={styles.userDetails}>
        {username ? (
          <>
            <Text style={styles.userDetailsHeader}>Username</Text>
            <Text style={styles.userDetailsText}>{username}</Text>
          </>
        ) : (
          <></>
        )}
        {profession ? (
          <>
            <Text style={styles.userDetailsHeader}>Profession</Text>
            <Text style={styles.userDetailsText}>{profession}</Text>
          </>
        ) : (
          <></>
        )}
        {jobTitle ? (
          <>
            <Text style={styles.userDetailsHeader}>Job Title</Text>
            <Text style={styles.userDetailsText}>{jobTitle}</Text>
          </>
        ) : (
          <></>
        )}
        {bio ? (
          <>
            <Text style={styles.userDetailsHeader}>About Yourself</Text>
            <Text style={styles.userDetailsText}>{bio}</Text>
          </>
        ) : (
          <></>
        )}
        {hourlyRateAda ? (
          <>
            <Text style={styles.userDetailsHeader}>Hourly Rate (ADA)</Text>
            <Text style={styles.userDetailsText}>{hourlyRateAda}</Text>
          </>
        ) : (
          <></>
        )}
        {skills ? (
          <>
            <Text style={styles.userDetailsHeader}>Skills</Text>
            <Text style={styles.userDetailsText}>{skills}</Text>
          </>
        ) : (
          <></>
        )}
      </View>
      <View style={styles.messageWrapper}>
        <Checkbox
          tag="accepted-tos-and-pp"
          colorMode="dark"
          onCheckBoxPress={onAcceptedTerms}
          acceptedCheckbox={acceptedTerms}
        />
        <View style={styles.messageTextWrapper}>
          <TextComponent>I've read and accept </TextComponent>
          <View style={styles.legalDocumentLinks}>
            <Pressable onPress={showTermsOfService}>
              <TextComponent isLink>Terms of Service</TextComponent>
            </Pressable>
            <TextComponent> and </TextComponent>
            <Pressable onPress={showPrivacyPolicy}>
              <TextComponent isLink>Privacy Policy</TextComponent>
            </Pressable>
          </View>
        </View>
      </View>
      {isBetaTestingActive && (
        <View style={styles.betaTesterCodeWrapper}>
          <SubHeaderText customStyle={styles.betaTesterInputLabel}>
            Have a Beta-Tester code? Fill it here:
          </SubHeaderText>
          <TextInput
            onChange={(e) => setBetaTesterCode(e.nativeEvent.text)}
            keyboardType="numeric"
            style={[formStyleDark.input, styles.betaTesterInputField]}
          />
        </View>
      )}
      <FullWidthButton
        disabled={!acceptedTerms}
        onPressCallback={onConfirm}
        colorScheme="dark"
        buttonType="transparent"
        loadingIndicator={isLoading}
        text="Confirm"
        isOnboarding
      />
      <SlideDownModal
        modalType={modalState.type}
        setModalState={setModalState}
        isVisibleModal={modalState.visible}
      />
    </KeyboardAwareScrollView>
  )
}

const styles = StyleSheet.create({
  container: {
    width: "90%",
    height: "100%",
    alignItems: "center",
  },
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
  subHeaderText: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.neutral,
  },
  innerTextCenter: {
    justifyContent: "center",
    padding: Sizing.x10,
  },
  image: {},
  userDetails: {
    width: "100%",
    minHeight: Sizing.x70,
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    padding: Sizing.x20,
  },
  betaTesterCodeWrapper: {
    alignItems: "center",
    justifyContent: "flex-end",
    marginVertical: Sizing.x12,
    flexDirection: "row",
  },
  betaTesterInputLabel: {
    flex: 1,
    textAlign: "right",
    marginRight: Sizing.x10,
  },
  betaTesterInputField: {
    flex: 1,
  },
  messageWrapper: {
    marginTop: Sizing.x10,
    marginBottom: Sizing.x5,
    flexDirection: "row",
  },
  messageTextWrapper: {},
  legalDocumentLinks: {
    flexDirection: "row",
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
})
