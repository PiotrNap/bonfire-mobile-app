import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import { Value } from "@hyperionbt/helios"
import { appContext, bookingContext, eventCreationContext } from "contexts/contextApi"
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
import { shareEvent, showErrorToast, showSuccessToast } from "lib/helpers"
import { unitsToAssets } from "lib/wallet/utils"
import { utf8ToHex } from "lib/utils"

export const DetailedConfirmation = ({ navigation, route }: any) => {
  const params = route?.params

  const { colorScheme } = appContext()
  const {
    textContent,
    selectedDays,
    fromDate,
    toDate,
    hourlyRate,
    imageURI,
    visibility,
    cancellation,
    eventCardColor,
    eventTitleColor,
    availabilities,
    gCalEventsBooking,
    resetEventCreationState,
  } = eventCreationContext()
  const { duration, pickedDate, previewingEvent, createGoogleCalEvent } = bookingContext()
  const { username, id } = React.useContext(ProfileContext)
  const {
    errorMsg,
    successMsg,
    isLoading: isEventDeletionLoading,
    deleteEvent,
  } = useEventDeletion(params.organizerEvent?.eventId)
  const [isLoading, setIsLoading] = React.useState<boolean>(false)

  const isLightMode = colorScheme === "light"
  const onBackNavigationPress = () => navigation.goBack()
  const onButtonPress = async () => {
    setIsLoading(true)

    if (params?.isNewEvent) {
      let _hourlyRate = [...hourlyRate]
      // convert AssetUnit[] to JSONSchema
      const hourlyRateAda = _hourlyRate[0].count // this one is required in UI
      _hourlyRate.shift()
      // return console.log(_hourlyRate)
      const hourlyRateValue = new Value(
        BigInt(hourlyRateAda),
        unitsToAssets(
          _hourlyRate.map((unit) => ({ ...unit, label: "", name: utf8ToHex(unit.name) }))
        )
      )
      const hourlyRateSchemaJSON = hourlyRateValue.toSchemaJson()

      const newEvent: CreateEventDto = {
        title: textContent.title,
        description: textContent.summary,
        availabilities,
        selectedDays,
        cancellation,
        fromDate,
        toDate,
        hourlyRate: hourlyRateSchemaJSON,
        visibility,
        eventCardColor,
        eventTitleColor,
        timeZoneOffset: new Date().getTimezoneOffset(),
        organizer: {
          id,
          username,
        },
        gCalEventsBooking,
      }

      try {
        const eventId = await Events.createEvent(newEvent)

        if (eventId && imageURI)
          // update img as we cant send multiple content-type headers
          await Events.uploadEventImage(imageURI, eventId)

        resetEventCreationState()
        navigation.navigate("Confirmation", {
          isBookingConfirmation: false,
          isNewEvent: true,
        })
      } catch (e) {
        if (e.response.status === 422) return showNSFWImageModal()
        showErrorToast(e)
      } finally {
        setIsLoading(false)
      }
    } else {
      try {
        const res = await Events.bookEvent({
          eventId: previewingEvent.eventId,
          durationCost: previewingEvent.durationCost,
          bookedDate: new Date(pickedDate),
          bookedDuration: duration,
          createGoogleCalEvent,
        })

        if (res) {
          navigation.navigate("Confirmation", {
            isBookingConfirmation: true,
          })
        }
      } catch (e) {
        showErrorToast(e)
        setIsLoading(false)
      }

      //@TODO submit transaction to blockchain
    }
  }
  const onDeleteEvent = async () => {
    if (!params.organizerEvent.eventId) return
    await deleteEvent()

    if (errorMsg) {
      showErrorToast(errorMsg)
    } else showSuccessToast("Success!", successMsg)
  }
  const onSharePress = async () => await shareEvent(params?.organizerCalendarEvent.id)

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode ? Colors.primary.neutral : Colors.neutral.s600,
        },
      ]}>
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
            {params?.header || "Confirmation"}
          </Text>
        </View>
        <ConfirmationDetails
          isCalendarEventPreview={params?.isCalendarEventPreview}
          organizerEvent={params?.organizerEvent || params?.organizerCalendarEvent}
          bookedEvent={params?.bookedEvent}
          isNewEvent={params?.isNewEvent}
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
        <View style={styles.buttonContainer}>
          {params?.organizerEvent && !params?.organizerEvent.numOfBookedSlots ? (
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
          ) : !params?.isCalendarEventPreview &&
            !params?.organizerCalendarEvent &&
            !params?.bookedEvent ? (
            <FullWidthButton
              onPressCallback={onButtonPress}
              text={"Confirm"}
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
            />
          ) : (
            <></>
          )}
        </View>
      </View>
    </SafeAreaView>
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
})
