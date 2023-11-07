import * as React from "react"
import { View, StyleSheet, Text, Pressable, useWindowDimensions } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import ViewPager from "react-native-pager-view"
import QRCode from "react-native-qrcode-svg"
import { CheckIcon, DuplicateIcon, LeftArrowIcon, PaymentIcon } from "icons/index"
import { Typography, Colors, Sizing, Outlines, Buttons } from "styles/index"
import { appContext, walletContext } from "contexts/contextApi"
import { BodyText } from "components/rnWrappers/bodyText"
import { ProfileContext } from "contexts/profileContext"
import { BigSlideModal } from "components/modals/BigSlideModal"
import { useNavigation } from "@react-navigation/native"
import Clipboard from "@react-native-clipboard/clipboard"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { monospace } from "../../styles/typography"
import { HeaderText } from "components/rnWrappers/headerText"

export interface WalletTopUpScreenProps {
  pagerRef?: React.RefObject<ViewPager>
  navigation?: any
  route?: any
}

export const ReceiveTransactionScreen = ({ route, pagerRef }: WalletTopUpScreenProps) => {
  const navigation = useNavigation()
  const { colorScheme } = appContext()
  const { baseAddress } = walletContext()
  const [isLightMode, setIsLightMode] = React.useState<boolean>(false)
  const [copyMsgActive, setCopyMsgActive] = React.useState<boolean>(false)
  const [isVisibleModal, setIsVisibleModal] = React.useState<boolean>(false)
  const windowWidth = useWindowDimensions().width

  const isRegistrationScreen = pagerRef?.current instanceof ViewPager

  const onBackNavigationPress = () => navigation.goBack()

  const hideModal = () => setIsVisibleModal(false)

  const WalletTopUpModal = React.useMemo(
    () => (
      <View>
        <BigSlideModal
          buttonCb={() => {}}
          hideModal={hideModal}
          isVisible={isVisibleModal}
          icon={<PaymentIcon width={windowWidth / 2} height={windowWidth / 2} />}
          header={"header"}
          body={"body"}
          buttonTitle={"buttonTitle"}
          secondButtonTitle={"secondButtonTitle"}
        />
      </View>
    ),
    [isVisibleModal]
  )

  const onCopyPress = () => {
    Clipboard.setString(baseAddress)
    setCopyMsgActive(true)
    setTimeout(() => setCopyMsgActive(false), 3000)
  }

  React.useEffect(() => {
    if (pagerRef?.current instanceof ViewPager) {
      setIsLightMode(false)
    } else {
      setIsLightMode(colorScheme === "light")
    }
  }, [colorScheme])

  return (
    <SafeAreaView
      style={[
        isRegistrationScreen
          ? styles.safeArea_dark
          : isLightMode
          ? styles.safeArea_light
          : styles.safeArea_dark,
      ]}>
      <View style={styles.mainContainer}>
        <KeyboardAwareScrollView
          keyboardShouldPersistTaps="handled"
          showsVerticalScrollIndicator={false}
          keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
          style={{ width: "100%" }}>
          {!isRegistrationScreen ? (
            <View style={styles.navigation}>
              <Pressable onPress={onBackNavigationPress} hitSlop={10}>
                <LeftArrowIcon
                  width={24}
                  height={24}
                  color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
                />
              </Pressable>
            </View>
          ) : (
            <View style={{ marginTop: Sizing.x20 }} />
          )}
          <View style={styles.header}>
            <HeaderText colorScheme={colorScheme}>{"Add Funds"}</HeaderText>
            <BodyText
              changingColorScheme
              colors={[Colors.primary.s600, Colors.primary.neutral]}>
              Fund your Bonfire wallet with ADA or other Cardano native assets using the
              address below.
            </BodyText>
          </View>
          <View style={styles.main}>
            <View style={styles.qrCodeContainer}>
              <QRCode
                value={baseAddress}
                size={windowWidth / 3}
                backgroundColor={Colors.primary.neutral}
              />
            </View>
            <View style={styles.addressWrapper}>
              <BodyText customColorScheme="light" customStyle={{ ...monospace.base }}>
                {baseAddress}
              </BodyText>
            </View>
            <Pressable onPress={onCopyPress} style={Buttons.applyOpacity(styles.copyBtn)}>
              {copyMsgActive ? (
                <CheckIcon stroke={Colors.primary.s800} style={styles.icon} />
              ) : (
                <Text style={styles.buttonText}>Copy</Text>
              )}
            </Pressable>
          </View>
        </KeyboardAwareScrollView>
        {isVisibleModal && WalletTopUpModal}
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea_light: {
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
    flex: 1,
  },
  safeArea_dark: {
    backgroundColor: Colors.neutral.s600,
    alignItems: "center",
    flex: 1,
  },
  mainContainer: {
    alignItems: "center",
    width: "90%",
  },
  container_light: {},
  container_dark: {},
  header: {
    width: "100%",
  },
  headerText_light: {
    ...Typography.header.x60,
    color: Colors.primary.s800,
    marginBottom: Sizing.x5,
  },
  headerText_dark: {
    ...Typography.header.x60,
    color: Colors.primary.neutral,
    marginBottom: Sizing.x5,
  },
  main: {
    justifyContent: "center",
    alignItems: "center",
    marginVertical: Sizing.x25,
  },
  qrCodeContainer: {
    padding: Sizing.x15,
    margin: Sizing.x20,
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
  },
  subHeaderText_light: {
    ...Typography.subHeader.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s600,
  },
  subHeaderText_dark: {
    ...Typography.subHeader.x30,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.neutral,
  },
  navigation: {
    flexDirection: "row",
    alignSelf: "flex-start",
    marginVertical: Sizing.x15,
  },
  buttonWrapper: {
    width: "100%",
    marginVertical: Sizing.x12,
  },
  backButtonSection: {
    marginTop: Sizing.x5,
    alignItems: "center",
    justifyContent: "center",
  },
  backButton: {
    flexDirection: "row",
    alignItems: "center",
    padding: Sizing.x10,
  },
  backButtonText: {
    lineHeight: 30,
    alignContent: "center",
    alignItems: "center",
    justifyContent: "center",
    paddingBottom: Sizing.x2,
    ...Typography.subHeader.x35,
    ...Typography.roboto.medium,
    color: Colors.primary.neutral,
  },
  backButtonIcon: {
    position: "absolute",
    left: -Sizing.x12,
  },
  buttonText: {
    ...Typography.header.x25,
    color: Colors.primary.s800,
    marginHorizontal: Sizing.x1,
    textAlign: "center",
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
    marginHorizontal: Sizing.x1,
  },
  copyBtn: {
    alignItems: "center",
    justifyContent: "center",
    flexDirection: "row",
    marginHorizontal: "auto",
    marginTop: Sizing.x10,
    width: Sizing.x100,
    height: Sizing.x45,
    backgroundColor: Colors.primary.s200,
    borderWidth: Outlines.borderWidth.thick,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.max,
  },
  addressWrapper: {
    marginTop: "auto",
    padding: Sizing.x10,
    backgroundColor: Colors.primary.neutral,
    borderWidth: Outlines.borderWidth.base,
    borderRadius: Outlines.borderRadius.base,
    borderColor: Colors.primary.s600,
  },
})
