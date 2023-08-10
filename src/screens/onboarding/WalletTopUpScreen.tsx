import * as React from "react"
import {
  View,
  StyleSheet,
  Text,
  Pressable,
  useWindowDimensions,
} from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import ViewPager from "react-native-pager-view"
import QRCode from "react-native-qrcode-svg"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { DuplicateIcon, LeftArrowIcon, PaymentIcon } from "icons/index"
import { Typography, Colors, Sizing, Outlines } from "styles/index"
import { appContext } from "contexts/contextApi"
import { BodyText } from "components/rnWrappers/bodyText"
import { ProfileContext } from "contexts/profileContext"
import { BigSlideModal } from "components/modals/BigSlideModal"
import { useNavigation } from "@react-navigation/native"
import Clipboard from "@react-native-clipboard/clipboard"
import { CopyMessage } from "components/popups/copyMessage"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"

export interface WalletTopUpScreenProps {
  pagerRef?: React.RefObject<ViewPager>
  navigation?: any
  route?: any
}

export const WalletTopUpScreen = ({
  route,
  pagerRef,
}: WalletTopUpScreenProps) => {
  const navigation = useNavigation()
  const { colorScheme, receivingAddr, textContent } = appContext()
  const [isLightMode, setIsLightMode] = React.useState<boolean>(false)
  const [address, setAddress] = React.useState<string>("")
  const [amount, setAmount] = React.useState<string>("")
  const [copyMsgActive, setCopyMsgActive] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [isVisibleModal, setIsVisibleModal] = React.useState<boolean>(false)
  const { hasSyncedWallet, walletBalance, setWalletBalance } =
    React.useContext(ProfileContext)
  const windowWidth = useWindowDimensions().width

  const isRegistrationScreen = pagerRef?.current instanceof ViewPager
  const isBookingScreen = route != null && route.params?.isBookingScreen

  const onTextChangeCallback = (value: any) => setAddress(value)
  const onAmountChangeCallback = (value: any) => setAmount(value)

  const onBackNavigationPress = () => navigation.goBack()

  const wait = (ms: number): Promise<void> =>
    new Promise((res) => setTimeout(res, ms))

  const processPayment = async () => {
    // if user hasn't synced any wallet yet:
    if (!hasSyncedWallet) return setIsVisibleModal(true)

    setIsLoading(true)
    //@TODO change in production
    await wait(2000)
    setWalletBalance(Number(walletBalance) + Number(amount))
    setIsLoading(false)

    // this means we are on the onboarding screen
    if (isRegistrationScreen) {
      navigation.navigate("Success", {
        fromScreen: "User Registration Screens",
      })
    }

    if (route != null && route.params?.fromScreen == "Duration Choice") {
      navigation.navigate("Success", {
        fromScreen: route.params.fromScreen,
      })
    }

    navigation.navigate("Success", {
      ...route.params,
      isBookingWalletTopUp: true,
    })
  }

  const hideModal = () => setIsVisibleModal(false)

  const WalletTopUpModal = React.useMemo(
    () => (
      <View>
        <BigSlideModal
          hideModal={hideModal}
          isVisible={isVisibleModal}
          icon={
            <PaymentIcon width={windowWidth / 2} height={windowWidth / 2} />
          }
          header={textContent.wallet.create_wallet.modal.header}
          body={textContent.wallet.create_wallet.modal.body}
          buttonTitle={textContent.wallet.create_wallet.modal.button_title}
          secondButtonTitle={
            textContent.wallet.create_wallet.modal.secondButton_title
          }
        />
      </View>
    ),
    [isVisibleModal]
  )

  const onCopyPress = () => {
    Clipboard.setString(address)
    setCopyMsgActive(true)
    setTimeout(() => setCopyMsgActive(false), 2000)
  }
  const onBackPress = () => {
    pagerRef?.current?.setPage(0)
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
                  color={
                    isLightMode ? Colors.primary.s600 : Colors.primary.neutral
                  }
                />
              </Pressable>
            </View>
          ) : (
            <View style={{ marginTop: Sizing.x20 }} />
          )}
          <View style={styles.header}>
            <Text
              style={[
                isLightMode ? styles.headerText_light : styles.headerText_dark,
              ]}>
              {"Add Funds"}
            </Text>
            <BodyText
              changingColorScheme
              colors={[Colors.primary.s600, Colors.primary.neutral]}>
              Fund your Bonfire wallet with ADA or Gimbals using the address
              below to make payments.
            </BodyText>
          </View>
          <View style={styles.main}>
            <View style={styles.qrCodeContainer}>
              <QRCode
                value={receivingAddr || "addr9czf30t9"}
                size={windowWidth / 2}
                backgroundColor={Colors.primary.neutral}
              />
            </View>
            <CustomPlainInput
              label="Address"
              //@TODO change to users address
              placeholder="addr9czf30t9..."
              icon={DuplicateIcon}
              labelStyle={!isBookingScreen && { color: Colors.primary.neutral }}
              onChangeCallback={onTextChangeCallback}
              onPressHandler={onCopyPress}
              customChild={<CopyMessage isActive={copyMsgActive} />}
              editable={false}
            />
            <CustomPlainInput
              label="Amount"
              placeholder="50"
              keyboardType="numeric"
              labelStyle={!isBookingScreen && { color: Colors.primary.neutral }}
              onChangeCallback={onAmountChangeCallback}
            />
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
    margin: Sizing.x10,
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
})
