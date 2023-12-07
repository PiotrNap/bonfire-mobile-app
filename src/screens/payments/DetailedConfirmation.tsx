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
  showSuccessToast,
} from "lib/helpers"
import { assetsUnitsToJSONSchema, assetsUnitsToValue } from "lib/wallet/utils"
import { AssetUnit } from "lib/wallet/types"
import { Authenticator } from "components/modals/Authenticator"
import { Wallet } from "lib/wallet"
import { Address } from "@hyperionbt/helios"
import { EscrowContractDatum } from "lib/wallet/types"
import { useWallet } from "lib/hooks/useWallet"
import { input, inputLabel, inputStyles } from "../../styles/forms"
import { CustomPlainInput } from "components/forms/CustomPlainInput"
import { Layout } from "components/layouts/basicLayout"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { fontWeight } from "../../styles/typography"

export const DetailedConfirmation = ({ navigation, route }: any) => {
  const params = route?.params

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
  const { walletUtxos, baseAddress } = walletContext()
  const { updateWalletBalance } = useWallet()
  const { username, id } = React.useContext(ProfileContext)
  const {
    errorMsg,
    successMsg,
    isLoading: isEventDeletionLoading,
    deleteEvent,
  } = useEventDeletion(params.organizerEvent?.eventId)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [authenticatorVisible, setAuthenticatorVisible] = React.useState<boolean>(false)
  const [eventNote, setEventNote] = React.useState<string | null>(null)
  const isLightMode = colorScheme === "light"
  const onHideAuthenticator = () => setAuthenticatorVisible(false)
  const onBackNavigationPress = () => navigation.goBack()

  /** Event booking **/
  const onStartBooking = () => {
    setAuthenticatorVisible(true)
  }
  const onAuthenticated = async (accountKey?: string | void) => {
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

    // create a locking transaction
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
  const onDeleteEvent = async () => {
    if (!params.organizerEvent.eventId) return
    await deleteEvent()

    if (errorMsg) {
      showErrorToast(errorMsg)
    } else showSuccessToast("Success!", successMsg)
  }
  const onSharePress = async () => await shareEvent(params?.organizerCalendarEvent.id)
  const onEvenNoteChange = (val) => setEventNote(val)

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
            {params?.bookingSlot ? "Details" : params?.header || "Confirmation"}
          </Text>
        </View>
        <ConfirmationDetails
          isCalendarEventPreview={params?.isCalendarEventPreview}
          organizerEvent={params?.organizerEvent || params?.organizerCalendarEvent}
          bookedEvent={params?.event}
          bookingSlot={params?.bookingSlot}
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
                Note to Your Customer
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
            !params?.bookedEvent ? (
            <FullWidthButton
              onPressCallback={params.event ? onStartBooking : onButtonPress}
              text={"Sign & Submit"}
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
            />
          ) : (
            <></>
          )}
        </View>
      </View>
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
