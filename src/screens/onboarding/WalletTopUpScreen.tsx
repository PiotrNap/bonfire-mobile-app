import * as React from "react";
import {
  View,
  StyleSheet,
  Text,
  Pressable,
  useWindowDimensions,
} from "react-native";

import { SafeAreaView } from "react-native-safe-area-context";
import ViewPager from "react-native-pager-view";
import QRCode from "react-native-qrcode-svg";
import { CustomPlainInput } from "components/forms/CustomPlainInput";
import { DuplicateIcon, LeftArrowIcon } from "icons/index";
import { Typography, Colors, Sizing, Outlines, Buttons } from "styles/index";
import { appContext } from "contexts/contextApi";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { BodyText } from "components/rnWrappers/bodyText";
import { ProfileContext } from "contexts/profileContext";
import { WalletSetUpModal } from "components/modals/walletSetUpModal";
import { useNavigation } from "@react-navigation/native";
import * as Clipboard from "expo-clipboard";
import { CopyMessage } from "components/popups/copyMessage";
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view";

export interface WalletTopUpScreenProps {
  pagerRef?: React.RefObject<ViewPager>;
  navigation?: any;
  route?: any;
}

export const WalletTopUpScreen = ({
  route,
  pagerRef,
}: WalletTopUpScreenProps) => {
  const navigation = useNavigation();
  const { colorScheme } = appContext();
  const [isLightMode, setIsLightMode] = React.useState<boolean>(false);
  const [address, setAddress] = React.useState<string>("");
  const [amount, setAmount] = React.useState<string>("");
  const [copyMsgActive, setCopyMsgActive] = React.useState<boolean>(false);
  const [isLoading, setIsLoading] = React.useState<boolean>(false);
  const [isVisibleModal, setIsVisibleModal] = React.useState<boolean>(false);
  const { hasSyncedWallet, walletBalance, setWalletBalance } =
    React.useContext(ProfileContext);
  const windowWidth = useWindowDimensions().width;
  const { fromScreen } = route.params;

  const isRegistrationScreen = pagerRef?.current instanceof ViewPager;
  const isBookingScreen = route != null && route.params?.isBookingScreen;
  const isDisabled = !address || !amount;

  const onTextChangeCallback = (value: any) => setAddress(value);
  const onAmountChangeCallback = (value: any) => setAmount(value);

  const onBackNavigationPress = () => navigation.goBack();

  const wait = (ms: number): Promise<void> =>
    new Promise((res) => setTimeout(res, ms));

  const processPayment = async () => {
    // if user hasn't synced any wallet yet:
    if (!hasSyncedWallet) return setIsVisibleModal(true);

    setIsLoading(true);
    //@TODO change in production
    await wait(2000);
    setWalletBalance(Number(walletBalance) + Number(amount));
    setIsLoading(false);

    // this means we are on the onboarding screen
    if (isRegistrationScreen) {
      navigation.navigate("Deposit Successful", {
        fromScreen: "User Registration Screens",
      });
    }

    if (route != null && route.params?.fromScreen == "Duration Choice") {
      navigation.navigate("Deposit Successful", {
        fromScreen: route.params.fromScreen,
      });
    }

    navigation.navigate("Deposit Successful", {
      ...route.params,
      isBookingWalletTopUp: true,
    });
  };

  const hideModal = () => setIsVisibleModal(false);

  const WalletTopUpModal = React.useMemo(
    () => (
      <View>
        <WalletSetUpModal hideModal={hideModal} isVisible={isVisibleModal} />
      </View>
    ),
    [isVisibleModal]
  );

  const onCopyPress = () => {
    Clipboard.default.setString(address);
    setCopyMsgActive(true);
    setTimeout(() => setCopyMsgActive(false), 2000);
  };
  const onBackPress = () => {
    pagerRef?.current?.setPage(0);
  };

  React.useEffect(() => {
    if (pagerRef?.current instanceof ViewPager) {
      setIsLightMode(false);
    } else {
      setIsLightMode(colorScheme === "light");
    }
  }, [colorScheme]);

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
              {isBookingScreen ? "Add funds" : "Set up your wallet"}
            </Text>
            {isBookingScreen ? (
              <BodyText colors={[Colors.primary.s600, Colors.primary.neutral]}>
                Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                Vestibulum venenatis quam sem, eget bibendum lorem convallis et.
                Donec velit ante, efficitur at ante eu, consequat hendrerit
                augue. Vivamus quis eros ex
              </BodyText>
            ) : (
              <Text
                style={[
                  isLightMode
                    ? styles.subHeaderText_light
                    : styles.subHeaderText_dark,
                ]}>
                Deposit /withdrawl ADA into your wallet to make payments, and
                receive payments.
              </Text>
            )}
          </View>
          <View style={styles.main}>
            <View style={styles.qrCodeContainer}>
              <QRCode
                value="https://gimbalabs.com"
                size={windowWidth / 2}
                backgroundColor={Colors.primary.neutral}
              />
            </View>
            <CustomPlainInput
              label="Address"
              placeholder="addr9czf30t9dzbdsxe79a2vtf8io"
              icon={DuplicateIcon}
              labelStyle={!isBookingScreen && { color: Colors.primary.neutral }}
              onChangeCallback={onTextChangeCallback}
              onPressHandler={onCopyPress}
              customChild={<CopyMessage isActive={copyMsgActive} />}
            />
            <CustomPlainInput
              label="Amount"
              placeholder="50 ₳"
              keyboardType="numeric"
              labelStyle={!isBookingScreen && { color: Colors.primary.neutral }}
              onChangeCallback={onAmountChangeCallback}
            />
            <View style={styles.buttonWrapper}>
              <FullWidthButton
                onPressCallback={processPayment}
                text={"Deposit"}
                colorScheme={colorScheme}
                disabled={isDisabled}
                loadingIndicator={isLoading}
                buttonType={!isLightMode ? "transparent" : "filled"}
                lightMode={isLightMode}
              />
            </View>
            {isRegistrationScreen && (
              <View style={styles.backButtonSection}>
                <Pressable
                  onPress={onBackPress}
                  style={Buttons.applyOpacity(styles.backButton)}>
                  <Text style={styles.backButtonText}>Back</Text>
                  <LeftArrowIcon
                    color={Colors.primary.neutral}
                    width={18}
                    height={18}
                    strokeWidth={3}
                    style={styles.backButtonIcon}
                  />
                </Pressable>
              </View>
            )}
          </View>
        </KeyboardAwareScrollView>
        {isVisibleModal && WalletTopUpModal}
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea_light: {
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
  },
  safeArea_dark: {
    backgroundColor: Colors.primary.s600,
    alignItems: "center",
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
    ...Typography.header.x65,
    color: Colors.primary.s800,
    marginBottom: Sizing.x5,
  },
  headerText_dark: {
    ...Typography.header.x65,
    color: Colors.primary.neutral,
    marginBottom: Sizing.x5,
  },
  main: {
    justifyContent: "center",
    alignItems: "center",
    marginVertical: Sizing.x25,
  },
  qrCodeContainer: {
    padding: Sizing.x25,
    backgroundColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
  },
  subHeaderText_light: {
    ...Typography.subHeader.x35,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.s600,
  },
  subHeaderText_dark: {
    ...Typography.subHeader.x35,
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
});
