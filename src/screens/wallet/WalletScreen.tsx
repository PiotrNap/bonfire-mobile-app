import * as React from "react"
import {
  View,
  Text,
  StyleSheet,
  Pressable,
  LayoutChangeEvent,
  ActivityIndicator,
  useWindowDimensions,
  Animated,
} from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import LinearGradient from "react-native-linear-gradient"
import { Buttons, Outlines, Typography, Sizing, Colors } from "styles/index"
import { StackScreenProps } from "@react-navigation/stack"
import { WalletStackParamList } from "common/types/navigationTypes"
import { appContext } from "contexts/contextApi"
import { PaymentIcon, RefreshIcon, SearchIcon } from "icons/index"
import { TransactionsList } from "components/wallet/transactionsList"
import { ProfileContext } from "contexts/profileContext"
import {
  Address,
  BaseAddress,
  Bip32PrivateKey,
} from "@emurgo/csl-mobile-bridge"
import { generateMnemonic } from "bip39"
import { Wallet } from "lib/wallet"
import { BigSlideModal } from "components/modals/BigSlideModal"
import { getFromEncryptedStorage } from "lib/encryptedStorage"
import { Checkbox } from "components/forms/Checkbox"
import { BodyText } from "components/rnWrappers/bodyText"
import { useWalletInit } from "lib/hooks/useWalletInit"

// @TODO: Implement navigationTypes type
export interface WalletScreenProps
  extends StackScreenProps<WalletStackParamList, "Wallet Main"> {}

export const WalletScreen = ({ navigation, route }: WalletScreenProps) => {
  const { colorScheme, textContent } = appContext()
  const { walletBalance, setWalletName, setWalletBaseAddress, walletName } =
    React.useContext(ProfileContext)
  const { generateMnemonic } = useWalletInit()
  const [layoutHeight, setLayoutHeight] = React.useState<any>(null)
  const [isSmallScreen, setIsSmallScreen] = React.useState<boolean>(false)
  const [isModalVisible, setIsModalVisible] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [checkboxesVisible, setCheckboxesVisible] =
    React.useState<boolean>(false)
  const [acceptedCheckbox, setAcceptedCheckbox] = React.useState<
    string | undefined
  >("")

  const animatedOpacity = React.useRef(new Animated.Value(0)).current
  const fadeIn = () => {
    Animated.timing(animatedOpacity, {
      toValue: 1,
      duration: 200,
      useNativeDriver: true,
    }).start(({ finished }) => {
      // if (finished) setAcceptedChecbox(true)
    })
  }
  const fadeOut = () => {
    Animated.timing(animatedOpacity, {
      toValue: 0,
      duration: 200,
      useNativeDriver: true,
    }).start(({ finished }) => {
      // if (finished) setAcceptedChecbox(false)
    })
  }

  React.useEffect(() => {
    if (checkboxesVisible) {
      fadeIn()
    } else fadeOut()
  }, [checkboxesVisible])

  React.useEffect(() => {
    ;(async () => {
      try {
        const _walletName = await getFromEncryptedStorage("wallet-name")
        const _walletBaseAddress = await getFromEncryptedStorage(
          "wallet-base-address"
        )

        setWalletName(_walletName)
        setWalletBaseAddress(_walletBaseAddress)
      } catch (e) {
        console.error(e)
      }
    })()

    const listener = navigation.addListener("blur", () => {
      hideModal()
    })

    return listener
  }, [route])

  const darkGradient: string[] = [Colors.primary.s800, Colors.primary.s600]
  const lightGradient: string[] = [Colors.primary.s200, Colors.primary.neutral]
  const windowWidth = useWindowDimensions().width

  const onAddWalletPress = () => {
    setIsModalVisible(true)
  }
  const onLayout = (e: LayoutChangeEvent) => {
    setLayoutHeight(e.nativeEvent.layout.height)

    if (layoutHeight && layoutHeight < 300) {
      setIsSmallScreen(true)
    }
  }
  const onBackPress = () => setCheckboxesVisible(false)
  const onWalletCreateNextPress = () => {
    if (acceptedCheckbox === "with-mnemonic") {
      setAcceptedCheckbox(undefined)
    } else if (acceptedCheckbox === "without-mnemonic") {
      setAcceptedCheckbox(undefined)
      navigation.navigate("New Wallet Set Up", {
        isNewWalletCreation: true,
        mnemonic: generateMnemonic(),
        createWalletSetupType: acceptedCheckbox,
      })
    }
  }
  const onCreateWalletPress = () => setCheckboxesVisible(true)
  const onImportWalletPress = () => navigation.navigate("Import Mnemonic")
  const hideModal = () => setIsModalVisible(false)
  const onCheckBoxPress = (tag: string | undefined) =>
    tag && setAcceptedCheckbox(tag)
  const onModalHide = () => {
    setAcceptedCheckbox("")
    setCheckboxesVisible(false)
    setIsModalVisible(false)
  }

  const modalComponent = React.useMemo(() => {
    return isModalVisible ? (
      <BigSlideModal
        hideModal={onModalHide}
        isVisible={isModalVisible}
        icon={<PaymentIcon width={windowWidth / 2} height={windowWidth / 2} />}
        header={textContent.wallet.add_new_wallet.modal.header}
        body={textContent.wallet.add_new_wallet.modal.body}
        buttonTitle={
          checkboxesVisible
            ? "Next Step"
            : textContent.wallet.add_new_wallet.modal.button_title
        }
        secondButtonTitle={
          checkboxesVisible
            ? "Back"
            : textContent.wallet.add_new_wallet.modal.secondButton_title
        }
        buttonCb={
          checkboxesVisible ? onWalletCreateNextPress : onImportWalletPress
        }
        secondButtonCb={checkboxesVisible ? onBackPress : onCreateWalletPress}
        buttonDisabled={!acceptedCheckbox && checkboxesVisible}>
        <Animated.View
          style={[styles.animatedView, { opacity: animatedOpacity }]}>
          <View style={styles.checkboxWrapper}>
            <Checkbox
              tag={"with-mnemonic"}
              acceptedCheckbox={acceptedCheckbox === "with-mnemonic"}
              onCheckBoxPress={onCheckBoxPress}>
              I want to write down my recovery phrase
            </Checkbox>
          </View>
          <View style={styles.checkboxWrapper}>
            <Checkbox
              tag={"without-mnemonic"}
              acceptedCheckbox={acceptedCheckbox === "without-mnemonic"}
              onCheckBoxPress={onCheckBoxPress}>
              I want my recovery phrase to be stored on this device (accessible
              later on)
            </Checkbox>
          </View>
        </Animated.View>
      </BigSlideModal>
    ) : (
      <></>
    )
  }, [isModalVisible, checkboxesVisible, acceptedCheckbox, animatedOpacity])

  return (
    <SafeAreaView
      style={[
        colorScheme == "light" ? styles.safeArea_light : styles.safeaArea_dark,
      ]}>
      <View style={styles.container}>
        <LinearGradient
          colors={colorScheme === "light" ? darkGradient : lightGradient}
          start={{ x: 0, y: 1 }}
          end={{ x: 1, y: 0 }}
          style={styles.walletContainer}>
          <ActivityIndicator
            animating={isLoading}
            color={
              colorScheme === "light"
                ? Colors.primary.neutral
                : Colors.primary.s800
            }
            style={styles.walletLoadingSpinner}
          />
          {!!walletName && (
            <Text
              style={[
                colorScheme == "light"
                  ? styles.walletHeader_ligth
                  : styles.walletHeader_dark,
                { marginRight: "auto" },
              ]}>
              {walletName}
            </Text>
          )}
          <Text
            style={[
              colorScheme == "light"
                ? styles.walletBalance_ligth
                : styles.walletBalance_dark,
            ]}>
            {!isLoading ? "--" : walletBalance} ₳
          </Text>
          <Pressable
            onPress={onAddWalletPress}
            pressRetentionOffset={15}
            hitSlop={15}
            style={Buttons.applyOpacity(
              colorScheme == "light"
                ? styles.walletButton_light
                : styles.walletButton_dark
            )}>
            <Text
              style={[
                colorScheme == "light"
                  ? styles.walletButtonText_light
                  : styles.walletButtonText_dark,
              ]}>
              {!isLoading ? "Add Wallet" : "Add funds"}
            </Text>
          </Pressable>
        </LinearGradient>
        <View onLayout={onLayout} style={styles.transactionsContainer}>
          <View style={styles.transactionsHeaderContainer}>
            <View style={styles.transactionsHeader}>
              <Text
                style={
                  colorScheme === "light"
                    ? styles.transactionsHeaderText_light
                    : styles.transactionsHeaderText_dark
                }>
                Transactions
              </Text>
              <Text
                style={
                  colorScheme === "light"
                    ? styles.transactionsSubheaderText_light
                    : styles.transactionsSubheaderText_dark
                }>
                Lorem Ipsum
              </Text>
            </View>
            {/*
            <View style={styles.iconWrapper}>
              {isSmallScreen && (
                <Pressable onPress={onRefreshPress} hitSlop={5}>
                  <RefreshIcon
                    width={22}
                    height={22}
                    stroke={
                      colorScheme === "light"
                        ? !isTxListLoading
                          ? Colors.primary.s600
                          : Colors.primary.s800
                        : Colors.primary.neutral
                    }
                    strokeWidth={!isTxListLoading ? 1.5 : 1.6}
                  />
                </Pressable>
              )}
              <SearchIcon
                width={24}
                height={24}
                stroke={
                  colorScheme === "light"
                    ? Colors.primary.s800
                    : Colors.primary.neutral
                }
                strokeWidth={1.8}
                style={{ marginLeft: "auto" }}
              />
            </View>
            */}
          </View>
          {/*
          <TransactionsList
            isLoading={isTxListLoading}
            isSmallScreen={isSmallScreen}
          />
          */}
          {modalComponent}
        </View>
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
  container: {
    flex: 1,
    width: "90%",
  },
  animatedView: {
    marginTop: Sizing.x10,
  },
  checkboxWrapper: {
    flexDirection: "row",
    alignItems: "center",
    marginVertical: Sizing.x5,
  },
  searchToolContainer: {
    alignItems: "flex-end",
    marginTop: Sizing.x10,
  },
  walletContainer: {
    height: Sizing.x130,
    alignItems: "center",
    justifyContent: "space-between",
    marginTop: Sizing.x20,
    padding: Sizing.x14,
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.lifted,
  },
  walletHeader_ligth: {
    ...Typography.header.x30,
    color: Colors.primary.s180,
  },
  walletHeader_dark: {
    ...Typography.header.x30,
    color: Colors.primary.s600,
  },
  walletBalance_ligth: {
    fontFamily: "Roboto-Medium",
    fontSize: Sizing.x65,
    color: Colors.primary.neutral,
  },
  walletBalance_dark: {
    fontFamily: "Roboto-Medium",
    fontSize: Sizing.x65,
    color: Colors.primary.s800,
  },
  walletButton_light: {
    paddingVertical: Sizing.x1,
    paddingHorizontal: Sizing.x8,
    borderWidth: Sizing.x3,
    borderColor: Colors.primary.neutral,
    borderRadius: Outlines.borderRadius.base,
    marginBottom: Sizing.x20,
    backgroundColor: "transparent",
  },
  walletButton_dark: {
    paddingVertical: Sizing.x2,
    paddingHorizontal: Sizing.x7,
    borderWidth: 4,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.base,
    marginBottom: Sizing.x20,
    backgroundColor: "transparent",
  },
  walletButtonText_light: {
    ...Typography.header.x25,
    textAlign: "center",
    color: Colors.primary.neutral,
  },
  walletButtonText_dark: {
    ...Typography.header.x30,
    textAlign: "center",
    color: Colors.primary.s800,
  },
  transactionsContainer: {
    flex: 1,
    marginTop: Sizing.x40,
  },
  transactionsHeaderContainer: {
    flexDirection: "row",
    alignItems: "center",
  },
  transactionsHeader: {},
  transactionsHeaderText_light: {
    ...Typography.header.x35,
    marginBottom: Sizing.x5,
    color: Colors.primary.s800,
  },
  transactionsHeaderText_dark: {
    ...Typography.header.x35,
    marginBottom: Sizing.x5,
    color: Colors.primary.neutral,
  },
  transactionsSubheaderText_light: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s600,
  },
  transactionsSubheaderText_dark: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s180,
  },
  iconWrapper: {
    width: "20%",
    flexDirection: "row",
    marginLeft: "auto",
    alignItems: "center",
  },
  walletLoadingSpinner: {
    position: "absolute",
    top: Sizing.x12,
    right: Sizing.x12,
  },
})
