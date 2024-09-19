import * as React from "react"
import { View, StyleSheet, Text, Dimensions, Pressable, Linking } from "react-native"

import { RegistrationIcon } from "icons/index"
import { Typography, Colors, Sizing, Outlines, Forms } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { appContext, walletContext } from "contexts/contextApi"
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
  const { addresses, resetSecrets, accountPubKeyHex } = walletContext()
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
  const { deviceTopInsent } = appContext()
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

  const showLegalDocument = async (type: "terms-of-service" | "privacy-policy") => {
    try {
      const url = WEBSITE_URL + "/" + type
      const browserResult = await openInAppBrowser(url)

      if (!browserResult) {
        if (type === "terms-of-service") {
          navigation.navigate("Legal Document", { type: "terms-of-service" })
        } else if (type === "privacy-policy") {
          navigation.navigate("Legal Document", { type: "privacy-policy" })
        } else throw new Error("Unknown type of Legal document")
      }
    } catch (e) {
      console.error(e)
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
      const user = await Users.createAccount({
        username,
        hourlyRateAda,
        bio,
        jobTitle,
        skills,
        profession,
        publicKey,
        walletPublicKey: accountPubKeyHex,
        baseAddresses: addresses,
      })

      if (!user || !user.id)
        throw new Error(
          `Something went wrong during registration. Please reload the app and try again`
        )

      const authResponseDTO = await startChallengeSequence(
        secretKey,
        user.deviceID,
        user.id
      )

      console.log(`
        adding to encrypted storage:
            deviceID: ${user.deviceID}
            authCred: ${authResponseDTO}
            devicePubKey: ${publicKey}
            deviceSecKey: ${secretKey}
            `)
      await setToEncryptedStorage("device-id", user.deviceID)
      await setToEncryptedStorage("auth-credentials", authResponseDTO)
      await setToEncryptedStorage("device-privKey", secretKey)
      await setToEncryptedStorage("device-pubKey", publicKey)

      setDeviceID(user.deviceID)
      setID(user.id)
      if (authResponseDTO?.accessToken) setAuthorizationToken(authResponseDTO.accessToken)

      // mint beta tester tokens if user registered successfully
      if (betaTesterCode) {
        let registered = await Users.registerForBetaTesting(betaTesterCode, user.id)
        if (!registered)
          throw new Error("Problem occured during BetaTester token minting.")
      }

      // clean up secret keys
      pagerRef.current.state = null
      resetSecrets()
      secretKey = ""
      publicKey = ""

      //@ts-ignore
      navigation.navigate("Navigation Screens")
    } catch (e) {
      showErrorToast({ error: e, topOffset: deviceTopInsent })
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
      <View style={styles.messageWrapper}>
        <Checkbox
          tag="accepted-tos-and-pp"
          colorMode="dark"
          onCheckBoxPress={onAcceptedTerms}
          acceptedCheckbox={acceptedTerms}
        />
        <View style={styles.messageTextWrapper}>
          <View style={{ flexDirection: "row", width: "100%", height: "auto" }}>
            <TextComponent>I've read and accept </TextComponent>
            <Pressable onPress={() => showLegalDocument("terms-of-service")}>
              <TextComponent isLink>Terms of Service</TextComponent>
            </Pressable>
            <TextComponent> and</TextComponent>
          </View>
          <View style={{ flexDirection: "row", width: "100%", height: "auto" }}>
            <Pressable onPress={() => showLegalDocument("privacy-policy")}>
              <TextComponent isLink>Privacy Policy</TextComponent>
            </Pressable>
          </View>
        </View>
      </View>
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
    textAlign: "left",
    alignItems: "flex-start",
    marginTop: Sizing.x15,
  },
  betaTesterInputLabel: {
    flex: 1,
    textAlign: "right",
  },
  betaTesterInputField: {
    flex: 1,
    marginTop: Sizing.x15,
  },
  messageWrapper: {
    marginTop: Sizing.x10,
    marginBottom: Sizing.x5,
    flexDirection: "row",
  },
  messageTextWrapper: {
    maxWidth: "auto",
  },
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
