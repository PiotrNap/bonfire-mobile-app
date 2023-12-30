import * as React from "react"
import {
  ActivityIndicator,
  FlatList,
  RefreshControl,
  StyleSheet,
  View,
} from "react-native"
import { StackScreenProps } from "@react-navigation/stack"
import { Layout } from "components/layouts/basicLayout"
import { TxInput } from "@hyperionbt/helios"

import { Colors, Sizing, Typography } from "styles/index"
import { appContext, walletContext } from "contexts/contextApi"
import { ProfileStackParamList } from "common/types/navigationTypes"
import { Authenticator } from "components/modals/Authenticator"
import { showErrorToast, showInfoToast } from "lib/helpers"
import { ProfileContext } from "contexts/profileContext"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { Events } from "Api/Events"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Checkbox } from "components/forms/Checkbox"
import { blockFrost, Wallet } from "lib/wallet"
import Crypto from "crypto"
import { AnyObject } from "yup/lib/types"
import {
  calculateFeeForTreasury,
  checkForBetaTesterToken,
  checkForCollateralAndFeeUtxos,
  COLLATERAL_LOVELACE,
  COLLATERAL_STORAGE_KEY,
  lovelaceToAda,
} from "lib/wallet/utils"
import { useWallet } from "lib/hooks/useWallet"
import { BigSlideModal } from "components/modals/BigSlideModal"
import AsyncStorage from "@react-native-async-storage/async-storage"
import { EventBookingSlot } from "common/types/dto"
import { PayoutsListItem } from "components/lists/myPayouts/payoutsListItem"

type ScreenProps = StackScreenProps<ProfileStackParamList, "Profile Settings">
type PayoutTxInfo = {
  payoutUtxo: TxInput
  spareUtxos: TxInput[]
  feeUtxo: TxInput
  collateralUtxo: TxInput
  hasBetaTesterToken: boolean
  serviceFee: BigInt
}
type MyPayoutTxType = "payout" | "collateral"

export const MyPayouts = ({ navigation }: ScreenProps) => {
  const { colorScheme, networkId } = appContext()
  const { id } = React.useContext(ProfileContext)
  const { walletUtxos, addresses } = walletContext()
  const networkBasedAddress =
    networkId === "Mainnet" ? addresses.mainnet : addresses.testnet
  useWallet() // this will re-fetch wallets utxo's

  const [authenticatorVisible, setAuthenticatorVisible] = React.useState<boolean>(false)
  const [availablePayouts, setAvailablePayouts] = React.useState<EventBookingSlot[]>([])
  const [selectedPayout, setSelectedPayout] = React.useState<AnyObject | null>(null)
  const [payoutTxInfo, setPayoutTxInfo] = React.useState<PayoutTxInfo | null>(null)
  const [isListLoading, setIsListLoading] = React.useState<boolean>(false)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [containerWidth, setContainerWidth] = React.useState<any>(0)
  const [selectedCheckbox, setSelectedCheckbox] = React.useState<number | null>(null)
  const [isCollateralPromptVisible, setIsCollateralPromptVisible] =
    React.useState<boolean>(false)
  const [txType, setTxType] = React.useState<MyPayoutTxType | "">("")

  const isLightMode = colorScheme === "light"

  const fetchUserAvailablePayouts = async () => {
    setIsListLoading(true)
    try {
      const queryResult = await Events.getBookingsByQuery({
        organizer_id: id,
        past_bookings: true,
        network_id: networkId,
      })
      if (!queryResult)
        throw new Error("Something went wrong while fetching the available payouts")

      const [results, count] = queryResult
      if (count > 0) setAvailablePayouts(results)
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsListLoading(false)
    }
  }

  React.useEffect(() => {
    fetchUserAvailablePayouts()
  }, [])

  const onButtonPress = async () => {
    if (!selectedPayout)
      return showInfoToast("Please select a payout to collect", "Can't proceed")
    if (!walletUtxos)
      return showInfoToast(
        "You don't seem to have enough funds in your wallet.",
        "Can't proceed"
      )

    setIsLoading(true)
    try {
      let _blockFrost = blockFrost(networkId)
      console.log("selectedPayout.lockingTxHash >", selectedPayout.lockingTxHash)
      // fetch utxos from selected transactions and get the utxo that holds the payment tokens
      const { data, error } = await Wallet.getTxUtxos(
        selectedPayout.lockingTxHash,
        networkId
      )
      if (error) return showErrorToast(error)

      const payoutUtxo = data.outputs.find(
        (utxo) => utxo.data_hash === selectedPayout.datumHash
      )
      //@ts-ignore because of method being internal
      const payoutTxIn: TxInput = await _blockFrost.restoreTxInput({
        ...payoutUtxo,
        tx_hash: data.hash,
      })

      const hasBetaTesterToken = checkForBetaTesterToken(walletUtxos)
      const serviceFee = hasBetaTesterToken ? 0n : calculateFeeForTreasury(payoutTxIn)
      const collateralUtxoId = await AsyncStorage.getItem(COLLATERAL_STORAGE_KEY)

      // console.log("hasBetaTesterToken >", hasBetaTesterToken)
      // console.log("serviceFee >", serviceFee)
      // console.log("collateralUtxoId >", collateralUtxoId)

      const { collateralUtxo, feeUtxo, spareUtxos, hasEnoughFunds, missingLovelace } =
        checkForCollateralAndFeeUtxos(
          walletUtxos,
          serviceFee,
          collateralUtxoId,
          networkId
        )
      if (!hasEnoughFunds)
        return showInfoToast(
          `You need ${lovelaceToAda(missingLovelace).toFixed(
            2
          )} more ADA to collect this payout.`,
          "Not enough funds"
        )
      if (!collateralUtxo) return setIsCollateralPromptVisible(true)
      if (!feeUtxo) throw Error("Missing tx-fee utxo.")

      setPayoutTxInfo({
        payoutUtxo: payoutTxIn,
        spareUtxos,
        collateralUtxo,
        feeUtxo,
        serviceFee,
        hasBetaTesterToken,
      })

      setAuthenticatorVisible(true)
      setTxType("payout")
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
    }
  }
  const onBackNavigationPress = () => navigation.goBack()
  const onAuthenticated = async (accountKey: string | void) => {
    setIsLoading(true)

    try {
      if (!accountKey)
        return showErrorToast(
          "Something went wrong. Have you enabled this option during registration?"
        )
      if (!selectedPayout) return showErrorToast("Have you selected a payout to collect?")

      if (txType === "payout") {
        if (!payoutTxInfo)
          return showErrorToast(
            "Unable to construct transaction. Missing payout tx-input."
          )
        const {
          payoutUtxo,
          spareUtxos,
          collateralUtxo,
          feeUtxo,
          serviceFee,
          hasBetaTesterToken,
        } = payoutTxInfo
        const { txHash } = await Wallet.sendPayoutTransaction(
          payoutUtxo,
          spareUtxos,
          collateralUtxo,
          feeUtxo,
          networkBasedAddress,
          accountKey,
          serviceFee,
          hasBetaTesterToken,
          networkId
        )
        console.log("txhash >", txHash)

        // update booking slot entity (isAvailable, unlockingTxHash , ..?)
        const updateBookingSlotDto = {
          id: selectedPayout.id,
          isActive: false,
          unlockingTxHash: txHash,
        }
        const updated = await Events.updateBookingById(
          selectedPayout.id,
          updateBookingSlotDto
        )

        if (updated)
          navigation.navigate("Confirmation", {
            customRoute: "Profile Main",
          })
      } else if (txType === "collateral") {
        const txHash = await Wallet.sendRegularTransaction(
          {
            assets: undefined,
            receiverAddress: networkBasedAddress,
            lovelace: COLLATERAL_LOVELACE,
          },
          networkBasedAddress,
          walletUtxos,
          accountKey,
          true, // this is a collateral split Tx
          networkId
        )
        if (!txHash) throw new Error("We were unable to submit the transaction")

        if (txHash) {
          navigation.navigate("Confirmation", {
            customRoute: "Profile Main",
          })
        }
      }
    } catch (e) {
      showErrorToast(e)
    } finally {
      accountKey = ""
      setIsLoading(false)
    }
  }
  const onHideAuthenticator = () => setAuthenticatorVisible(false)
  const onLayout = (e) => setContainerWidth(e.nativeEvent.layout.width)

  //const getItemLayout = React.useCallback(
  //  (_, index: number) => ({
  //    length: containerWidth,
  //    //@TODO change this to what is set eventually
  //    offset: (Sizing.x55 + Sizing.x25) * index,
  //    index,
  //  }),
  //  [containerWidth]
  //)
  const onCheckBoxPress = (idx: number) => {
    if (idx === selectedCheckbox) {
      setSelectedPayout(null)
      setSelectedCheckbox(null)
    } else {
      setSelectedPayout(availablePayouts[idx])
      setSelectedCheckbox(idx)
    }
  }

  const keyExtractor = (item: EventBookingSlot) => item.id

  const refreshControl = (
    <RefreshControl
      tintColor={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
      refreshing={isListLoading}
      onRefresh={fetchUserAvailablePayouts}
    />
  )

  const renderItem = ({ item, index }: any) => (
    <PayoutsListItem
      onCheckBoxPress={onCheckBoxPress}
      isSelected={selectedCheckbox === index}
      index={index}
      item={item}
    />
  )

  const initiateCollateralSplit = () => {
    setTxType("collateral")
    setIsCollateralPromptVisible(false)
    setAuthenticatorVisible(true)
  }
  const closeCollateralPrompt = () => setIsCollateralPromptVisible(false)

  return (
    <Layout backNavigationIcon backNavigationCb={onBackNavigationPress}>
      <View style={styles.mainContainer}>
        {isListLoading ? (
          <ActivityIndicator
            animating={isLoading}
            size="large"
            color={colorScheme === "light" ? Colors.primary.neutral : Colors.primary.s800}
          />
        ) : availablePayouts.length ? (
          <FlatList
            onLayout={onLayout}
            contentContainerStyle={isListLoading ? { opacity: 0.5 } : {}}
            refreshControl={refreshControl}
            extraData={selectedCheckbox}
            data={availablePayouts}
            keyExtractor={keyExtractor}
            renderItem={renderItem}
            scrollEnabled={!isLoading}
            disableScrollViewPanResponder={isLoading}
            onEndReachedThreshold={0.3}
            windowSize={8}
          />
        ) : (
          <SubHeaderText
            customStyle={styles.noPayoutsText}
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            You don't have any payouts available at the moment
          </SubHeaderText>
        )}
      </View>
      <View style={styles.bottomContainer}>
        <FullWidthButton
          onPressCallback={onButtonPress}
          text={"Collect"}
          colorScheme={colorScheme}
          disabled={isListLoading || selectedPayout === null}
          loadingIndicator={isLoading}
        />
      </View>
      {isCollateralPromptVisible && (
        <BigSlideModal
          header="Collateral Required"
          body="Your wallet doesn't have a UTxO which can be used as a collateral. We can submit a UTxO split transaction for you to make one available (we will keep re-using this UTxO)."
          isVisible={isCollateralPromptVisible}
          hideModal={closeCollateralPrompt}
          buttonTitle="Accept"
          secondButtonTitle="Cancel"
          buttonCb={initiateCollateralSplit}
          secondButtonCb={closeCollateralPrompt}
          customStyles={styles.collateralModal}
        />
      )}
      {authenticatorVisible && (
        <Authenticator
          authRequestType="account-key"
          showAuthenticator={authenticatorVisible}
          onAuthenticatedCb={onAuthenticated}
          onHideAuthenticatorCb={onHideAuthenticator}
        />
      )}
    </Layout>
  )
}

const styles = StyleSheet.create({
  mainContainer: { flex: 1, width: "90%" },
  bottomContainer: { width: "90%", margin: Sizing.x10 },
  noPayoutsText: {
    alignSelf: "center",
    textAlign: "center",
    ...Typography.subHeader.x25,
    marginTop: Sizing.x15,
  },
  collateralModal: {
    height: "auto",
  },
})
