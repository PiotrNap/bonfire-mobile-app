import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import {
  appContext,
  bookingContext,
  eventCreationContext,
  walletContext,
} from "contexts/contextApi"
import { LeftArrowIcon, ShareIcon } from "assets/icons"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { ConfirmationDetails } from "components/booking"
import { ProfileContext } from "contexts/profileContext"
import { CreateEventDto } from "common/types/dto/create-event.dto"
import { useEventDeletion } from "lib/hooks/useEventDeletion"
import { Events } from "Api/Events"
import { showNSFWImageModal } from "lib/modalAlertsHelpers"
import { SmallButton } from "components/buttons/smallButton"
import {
  convertToEventAvailabilityUTC,
  findEarliestAndLatestDates,
  shareEvent,
  showErrorToast,
  showInfoToast,
  showSuccessToast,
} from "lib/helpers"
import {
  assetsUnitsToJSONSchema,
  assetsUnitsToValue,
  calculateCancellationFee,
  checkForCollateralAndFeeUtxos,
  COLLATERAL_LOVELACE,
  lovelaceToAda,
} from "lib/wallet/utils"
import { AssetUnit } from "lib/wallet/types"
import { Authenticator } from "components/modals/Authenticator"
import { blockFrost, Wallet } from "lib/wallet"
import { Address, TxInput, Value } from "@hyperionbt/helios"
import { EscrowContractDatum } from "lib/wallet/types"
import { useWallet } from "lib/hooks/useWallet"
import { input, inputLabel } from "../../styles/forms"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { Layout } from "components/layouts/basicLayout"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { fontWeight } from "../../styles/typography"
import { BigSlideModal } from "components/modals/BigSlideModal"

export const DetailedConfirmation = ({ navigation, route }: any) => {
  const params = route?.params
  useWallet() // reload wallet utxos

  const { colorScheme } = appContext()
  const {
    textContent,
    selectedDates,
    hourlyRate,
    imageURI,
    visibility,
    cancellation,
    eventCardColor,
    eventTitleColor,
    availabilities,
    resetEventCreationState,
  } = eventCreationContext()
  const { duration, durationCost, pickedStartTime } = bookingContext()
  const { username, id, collateralUtxoId } = React.useContext(ProfileContext)
  const { walletUtxos, baseAddress } = walletContext()
  const { updateWalletBalance } = useWallet()
  const {
    errorMsg,
    successMsg,
    isLoading: isEventDeletionLoading,
    deleteEvent,
  } = useEventDeletion(params.organizerEvent?.eventId)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [authenticatorVisible, setAuthenticatorVisible] = React.useState<boolean>(false)
  const [eventNote, setEventNote] = React.useState<string | null>(null)
  const [isCollateralPromptVisible, setIsCollateralPromptVisible] =
    React.useState<boolean>(false)
  const [isCollateralTransaction, setIsCollateralTransaction] =
    React.useState<boolean>(false)
  const [cancellationTxInfo, setCancellationTxInfo] = React.useState<any>(null)

  const isLightMode = colorScheme === "light"
  const bookingSlot = params?.bookingSlot
  const isCancellableBookingSlot =
    bookingSlot && new Date() < new Date(bookingSlot?.fromDate)

  const onHideAuthenticator = () => setAuthenticatorVisible(false)
  const onBackNavigationPress = () => navigation.goBack()
  const onSharePress = async () => await shareEvent(params?.organizerCalendarEvent.id)
  const onEvenNoteChange = (val) => setEventNote(val)

  /** Event booking **/
  const onStartBooking = () => {
    setAuthenticatorVisible(true)
  }
  const bookEvent = async (accountKey?: string | void) => {
    if (!accountKey) return showErrorToast("Something went wrong. Missing signing key.")

    // Temporary solution...
    const lovelacePaymentToken: AssetUnit = {
      count: Number(durationCost.get("lovelace")),
      label: "",
      name: "",
      policyId: "",
    }
    const assetsPaymentTokens = Array.from(durationCost.values())
    assetsPaymentTokens.shift()
    const paymentTokens = assetsUnitsToValue(lovelacePaymentToken, assetsPaymentTokens)

    const lockingDatumInfo: EscrowContractDatum = {
      beneficiaryPkh: new Address(params.event.organizerAddress).pubKeyHash?.hex,
      benefactorPkh: new Address(baseAddress).pubKeyHash?.hex,
      releaseDate: BigInt(Math.floor(new Date(pickedStartTime).getTime() + duration)),
      cancelFee: params.event.cancellation.fee || 0,
      cancelWindowStart: BigInt(
        Math.floor(
          new Date(pickedStartTime).getTime() -
            params.event.cancellation.window * 60 * 60 * 1000
        )
      ),
      cancelWindowEnd: BigInt(Math.floor(new Date(pickedStartTime).getTime())),
      createdAt: BigInt(Math.floor(new Date().getTime())),
      paymentTokens: paymentTokens.toSchemaJson(),
    }

    if (Object.values(lockingDatumInfo).some((v) => v == null || v === ""))
      return showErrorToast("Unable to construct Datum object.", "Error")
    try {
      // submit transaction
      const { txHash, datumHash } = await Wallet.sendLockingTransaction(
        paymentTokens,
        lockingDatumInfo,
        baseAddress,
        walletUtxos,
        accountKey
      )

      // create new booking-record
      const res = await Events.bookEvent({
        lockingTxHash: txHash,
        eventId: params.event.id,
        durationCost: paymentTokens.toSchemaJson(),
        startDate: pickedStartTime,
        duration,
        datumHash,
      })

      if (res) {
        navigation.navigate("Confirmation", {
          isBookingConfirmation: true,
        })
      }
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
      accountKey = ""
      updateWalletBalance() // update wallet because there may be stale utxos
    }
  }
  /***/

  /** Event creation **/
  const onButtonPress = async () => {
    // convert selectedDates and availabilities to the new format
    let eventAvailableSlots = convertToEventAvailabilityUTC(selectedDates, availabilities)
    const { earliestDate, latestDate } = findEarliestAndLatestDates(eventAvailableSlots)

    const hourlyRateAssets = Array.from(hourlyRate)
    const lovelaceUnitAsset = hourlyRateAssets[0]
    hourlyRateAssets.shift()

    const newEvent: CreateEventDto = {
      title: textContent.title,
      description: textContent.summary,
      availabilities: eventAvailableSlots,
      cancellation,
      fromDate: earliestDate,
      toDate: latestDate,
      hourlyRate: assetsUnitsToJSONSchema(lovelaceUnitAsset, hourlyRateAssets),
      visibility,
      eventCardColor,
      eventTitleColor,
      note: eventNote || "",
      organizer: {
        id,
        username,
        baseAddress,
      },
    }

    try {
      setIsLoading(true)
      const eventId = await Events.createEvent(newEvent)

      // update img as we cant send multiple content-type headers
      if (eventId && imageURI) await Events.uploadEventImage(imageURI, eventId)

      resetEventCreationState()
      navigation.navigate("Confirmation", {
        isBookingConfirmation: false,
        isNewEvent: true,
      })
    } catch (e) {
      if (e.response?.status === 422) return showNSFWImageModal()
      showErrorToast(e)
    } finally {
      setIsLoading(false)
    }
  }
  /***/

  /** Event deletion **/
  const onDeleteEvent = async () => {
    if (!params.organizerEvent.eventId) return
    await deleteEvent()

    if (errorMsg) {
      showErrorToast(errorMsg)
    } else showSuccessToast("Success!", successMsg)
  }
  /***/

  /** Event cancellation **/
  const onCancelEventPress = () => {
    const {
      isWithinCancellationWindow,
      isBeforeCancellationWindow,
      cancellationFeeLovelace,
    } = calculateCancellationFee(
      bookingSlot.fromDate,
      bookingSlot.cancellation.window,
      bookingSlot.cancellation.fee,
      bookingSlot.cost
    )
    console.log("cancellationFeeLovelace >", cancellationFeeLovelace)
    const { collateralUtxo, feeUtxo, spareUtxos, hasEnoughFunds, missingLovelace } =
      checkForCollateralAndFeeUtxos(
        walletUtxos,
        cancellationFeeLovelace,
        collateralUtxoId
      )

    // check if after selecting a utxo to cover the collateral,
    // there is still enough to cover the cancellation fee + onchain fee
    // const enoughValueInSpareUtxos = (spareUtxos as TxInput[])
    //   .reduce((prev, acc) => prev.add(acc.value), new Value())
    //   .gt(new Value(MIN_VALIDATOR_FEE + cancellationFeeLovelace))

    console.log("feeUtxo >", feeUtxo?.value.dump())
    console.log("hasEnoughFunds >", hasEnoughFunds)
    console.log("collateralUtxoId >", collateralUtxoId)
    console.log("collateralUtxo >", collateralUtxo?.output.dump())
    if (!hasEnoughFunds)
      return showInfoToast(
        `You need ${lovelaceToAda(missingLovelace).toFixed(
          2
        )} more ADA to cancel this event.`,
        "Not enough funds"
      )

    if (!collateralUtxo) return setIsCollateralPromptVisible(true)
    if (!feeUtxo) throw Error("Missing tx-fee utxo.")

    setCancellationTxInfo({
      spareUtxos,
      feeUtxo,
      collateralUtxo,
      cancellationFeeValue: new Value(cancellationFeeLovelace),
      isBeforeCancellationWindow,
      isWithinCancellationWindow,
    })
    setAuthenticatorVisible(true)
  }
  const cancelEvent = async (accountKey: string | void) => {
    if (!accountKey)
      return showErrorToast(
        "Something went wrong. Have you enabled this option during registration?"
      )
    if (!cancellationTxInfo)
      return showErrorToast("We're unable to construct a cancellation tx")
    const {
      spareUtxos,
      feeUtxo,
      collateralUtxo,
      isBeforeCancellationWindow,
      cancellationFeeValue,
    } = cancellationTxInfo

    setIsLoading(true)
    try {
      let _blockFrost = blockFrost()
      const { data, error } = await Wallet.getTxUtxos(bookingSlot.lockingTxHash)
      if (error) return showErrorToast(error)

      const lockedUtxo = data.outputs.find(
        (utxo) => utxo.data_hash === bookingSlot.datumHash
      )
      //@ts-ignore because of method being internal
      const lockedTxIn: TxInput = await _blockFrost.restoreTxInput({
        ...lockedUtxo,
        tx_hash: data.hash,
      })

      const isEventOrganizer = bookingSlot.organizerId === id
      const { txHash } = await Wallet.sendCancellationTransaction(
        lockedTxIn,
        spareUtxos,
        collateralUtxo,
        feeUtxo,
        cancellationFeeValue,
        isEventOrganizer ? baseAddress : bookingSlot.organizer.baseAddress, // beneficiary addr
        isEventOrganizer ? bookingSlot.attendee.baseAddress : baseAddress, // benefactor addr
        accountKey,
        isEventOrganizer,
        isBeforeCancellationWindow
      )
      if (!txHash) return showErrorToast("Something went wrong during tx submittion")

      const res = await Events.deleteEventBooking(bookingSlot.id, {
        ...(isEventOrganizer ? { organizer_id: id } : { attendee_id: id }),
        txHash,
      })
      if (!res) throw new Error("Something went wrong while updating this record")

      navigation.navigate("Confirmation", {
        customRoute: "User Events",
        reload: true,
      })
    } catch (e) {
      showErrorToast(e)
    } finally {
      accountKey = ""
      setIsLoading(false)
    }
  }
  /***/

  /** Collateral split **/
  const makeCollateralSplit = async (accountKey: string | void) => {
    if (!accountKey)
      return showErrorToast(
        "Something went wrong. Have you enabled this option during registration?"
      )

    setIsLoading(true)
    try {
      const txHash = await Wallet.sendRegularTransaction(
        {
          assets: undefined,
          receiverAddress: baseAddress,
          lovelace: COLLATERAL_LOVELACE,
        },
        baseAddress,
        walletUtxos,
        accountKey,
        true // this is a collateral split Tx
      )
      if (!txHash) throw new Error("We were unable to submit the transaction")

      if (txHash) {
        navigation.navigate("Confirmation", {
          customRoute: "User Events",
          reload: true,
        })
      }
    } catch (e) {
      showErrorToast(e)
    } finally {
      accountKey = ""
      setIsLoading(false)
      setIsCollateralTransaction(false)
    }
  }
  const initiateCollateralSplit = () => {
    setIsCollateralTransaction(true)
    setIsCollateralPromptVisible(false)
    setAuthenticatorVisible(true)
  }
  const closeCollateralPrompt = () => {
    setIsCollateralPromptVisible(false)
  }
  /***/

  return (
    <Layout scrollable>
      <View style={[styles.container, { flex: 1 }]}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <View style={styles.header}>
          <Text style={isLightMode ? styles.headerText_light : styles.headerText_dark}>
            {bookingSlot ? "Details" : params?.header || "Confirmation"}
          </Text>
        </View>
        <ConfirmationDetails
          isCalendarEventPreview={params?.isCalendarEventPreview}
          organizerEvent={params?.organizerEvent || params?.organizerCalendarEvent}
          bookedEvent={params?.event}
          bookingSlot={bookingSlot}
          bookingSlotType={params?.bookingSlotType}
          isNewEvent={params?.isNewEvent}
          withFlatList={false}
        />
        {params?.organizerCalendarEvent && (
          <View style={styles.shareButtonWrapper}>
            <SmallButton
              title="Share Event"
              icon={
                <ShareIcon
                  color={Colors.primary.neutral}
                  width={Sizing.x14}
                  height={Sizing.x14}
                  strokeWidth={4}
                />
              }
              onPress={onSharePress}
            />
          </View>
        )}
        {params?.isNewEvent && (
          <View style={styles.noteContainer}>
            <View style={styles.headerContent}>
              <SubHeaderText
                colors={[Colors.primary.s800, Colors.primary.neutral]}
                customStyle={{ marginRight: "auto", ...fontWeight.bold }}>
                Note to Your Customers
              </SubHeaderText>
            </View>
            <CustomPlainInput
              key="note"
              multiline={true}
              numberOfLines={8}
              maxChar={300}
              placeholder={
                typeof eventNote !== "string"
                  ? "Thanks for scheduling time w/ me! To get in touch ..."
                  : ""
              }
              onChangeCallback={onEvenNoteChange}
            />
          </View>
        )}
        <View style={styles.buttonContainer}>
          {params?.organizerEvent && !params?.organizerEvent.numOfBookedSlots ? (
            //@TODO check if it works when there are booked slots
            !successMsg ? (
              <FullWidthButton
                onPressCallback={onDeleteEvent}
                text="Close Event"
                colorScheme={colorScheme}
                loadingIndicator={isEventDeletionLoading}
                textStyle={{ color: Colors.primary.neutral }}
                style={{
                  backgroundColor: Colors.danger.s300,
                  borderColor: Colors.danger.s300,
                }}
              />
            ) : (
              <></>
            )
          ) : params.isNewEvent ? (
            <FullWidthButton
              onPressCallback={onButtonPress}
              text={"Confirm"}
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
            />
          ) : !params?.isCalendarEventPreview &&
            !params?.organizerCalendarEvent &&
            !params?.bookedEvent &&
            !bookingSlot ? (
            <FullWidthButton
              onPressCallback={params.event ? onStartBooking : onButtonPress}
              text={"Sign & Submit"}
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
            />
          ) : bookingSlot && isCancellableBookingSlot ? (
            <FullWidthButton
              onPressCallback={onCancelEventPress}
              text={"Cancel"}
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
              customBgColor={Colors.danger.s300}
            />
          ) : (
            <></>
          )}
        </View>
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
          onAuthenticatedCb={
            isCollateralTransaction
              ? makeCollateralSplit
              : params.event
              ? bookEvent
              : cancelEvent
          }
          onHideAuthenticatorCb={onHideAuthenticator}
        />
      )}
    </Layout>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  container: {
    width: "90%",
  },
  navigation: {
    flexDirection: "row",
    marginVertical: Sizing.x15,
    alignSelf: "flex-start",
  },
  header: {
    alignSelf: "flex-start",
  },
  headerText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  headerText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
  detailsWrapper: {
    flex: 1,
    height: "100%",
  },
  buttonContainer: {
    width: "100%",
    marginTop: "auto",
    marginBottom: Sizing.x15,
  },
  shareButtonWrapper: {
    marginTop: "auto",
    justifyContent: "center",
    alignItems: "center",
  },
  shareEventButton: {
    borderRadius: Outlines.borderRadius.base,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    paddingVertical: Sizing.x5,
    paddingHorizontal: Sizing.x10,
    ...Outlines.shadow.base,
  },
  shareEventButtonText: {
    ...Typography.header.x20,
    marginRight: Sizing.x5,
  },
  noteContainer: {
    marginTop: "auto",
    marginHorizontal: Sizing.x5,
    paddingVertical: Sizing.x15,
  },
  headerContent: {
    flexDirection: "row",
    justifyContent: "space-between",
    width: "100%",
    marginBottom: 4,
  },
  collateralModal: {
    height: "auto",
  },
})

export const formStyleLight = StyleSheet.create({
  label: {
    ...inputLabel.primary_light,
  },
  input: {
    width: "100%",
    ...input.primary_light,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
})

export const formStyleDark = StyleSheet.create({
  label: {
    ...inputLabel.primary_dark,
  },
  input: {
    width: "100%",
    ...input.primary_dark,
    ...Outlines.shadow.lifted,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
})
