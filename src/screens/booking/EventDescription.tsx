import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { useSafeAreaInsets } from "react-native-safe-area-context"
import { applyOpacity } from "../../styles/colors"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import {
  availableDaysLeftInCurrMonth,
  convertToCalendarAvailabilities,
  getEventCardDate,
} from "lib/utils"
import { LeftArrowIcon, ShareIcon, UserIcon } from "assets/icons"
import { appContext, bookingContext, myCalendarContext } from "contexts/contextApi"
import { BodyText } from "components/rnWrappers/bodyText"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import tinyColor from "tinycolor2"
import { Events } from "Api/Events"
import { ProfileContext } from "contexts/profileContext"
import FastImage from "react-native-fast-image"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { shareEvent, showErrorToast, showSuccessToast } from "lib/helpers"
import LinearGradient from "react-native-linear-gradient"
import {
  assetsToUnitsArray,
  hexToUtf8,
  lovelaceToAda,
  schemaToPaymentTokens,
} from "lib/wallet/utils"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { useEventDeletion } from "lib/hooks/useEventDeletion"

export const EventDescription = ({ navigation, route }: any) => {
  const {
    title,
    description,
    eventId,
    organizerId,
    organizerAlias,
    hourlyRate: hourlyRateJSONSchema,
    fromDate,
    toDate,
    image,
    color,
    titleColor,
    isStandardColor,
    bookedSlots, // is this passed correctly?
  } = route.params
  const { colorScheme } = appContext()
  const { id } = React.useContext(ProfileContext)
  const { isLoading: isEventDeletionLoading, deleteEvent } = useEventDeletion(eventId)

  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [isEventOwner, setIsEventOwner] = React.useState<boolean>(false)
  const insets = useSafeAreaInsets()
  const isLightMode = colorScheme === "light"
  const _color = tinyColor(color).toHexString()
  const eventHourlyRate = React.useCallback(() => {
    const { lovelace, assets } = schemaToPaymentTokens(hourlyRateJSONSchema)

    return { ada: lovelaceToAda(BigInt(lovelace)), assets: assetsToUnitsArray([assets]) }
  }, [hourlyRateJSONSchema])

  console.log(eventHourlyRate())

  const onBackNavigationPress = () => navigation.goBack()
  const onBookEventPress = async () => {
    setIsLoading(true)

    try {
      // @TODO (s)
      // 1. check if user can pay for the event
      // 2. check whether there are still open time slots
      // 3. adjust the event time and availabilities to match users' own time-zone
      // 4. check whether deep-linking navigation still working

      // We have all event info from previous fetch when navigating with deep link
      // if (route.params?.fromShareLink) {
      //   const { fromShareLink, ...rest } = route.params
      //   event = rest
      // } else {
      //   event = await Events.getEventById(eventId)
      // }

      let event = await Events.getEventById(eventId)
      // return console.log(JSON.stringify(event, null, 4))
      if (!event) return

      // if (!availableDaysLeftInCurrMonth(Object.values(event.selectedDays))) {
      //   const nextAvailableMonth: number | undefined = (
      //     Object.values(event.selectedDays) as number[]
      //   ).find(
      //     (d: any) =>
      //       dayjs(d).month() > dayjs().month() && dayjs(d).year() >= dayjs().year()
      //   )
      //   if (!nextAvailableMonth) return

      //   const availableStartDate = dayjs(nextAvailableMonth)
      //   const calendarSetup = {
      //     nextMonths: true,
      //     month: availableStartDate.month(),
      //     year: availableStartDate.year(),
      //     isBookingCalendar: true,
      //     isRegularCalendar: false,
      //     availabilities: availableDays,
      //     startFromCustomMonth: true,
      //     isNewCalendar: true,
      //   }
      //   loadMyCalendar(calendarSetup)
      //   changeMonthHeader({
      //     month: months[availableStartDate.month()],
      //     year: availableStartDate.year(),
      //     numOfEvents: 0,
      //     startingDate: new Date(availableStartDate.year(), availableStartDate.month()),
      //   })
      // } else {
      //   setAvailCalendar(availableDays)
      // }
      // setPreviewingEvent(Object.assign({}, event, route.params))

      navigation.navigate("Available Event Dates Selection", {
        event,
      })
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
    }
  }

  // const onEventDetailPreview = () =>
  //   navigation.navigate("Event Details", {
  //     header: "Details",
  //     organizerEvent: { ...route.params },
  //   })
  const onSharePress = async () => await shareEvent(eventId)
  const onDeleteEvent = async () => {
    try {
      if (!eventId) return
      await deleteEvent()
      showSuccessToast("Success!", "This event was removed.")
    } catch (e) {
      showErrorToast(e)
    } finally {
      navigation.navigate("User Events")
    }
  }

  React.useEffect(() => {
    if (id && organizerId && id === organizerId) setIsEventOwner(true)
  }, [])

  const gradient: string[] = !isStandardColor
    ? [_color, _color]
    : [Colors.primary.s800, Colors.primary.s600]

  const Background = React.useCallback(
    ({ children }) =>
      image ? (
        <FastImage
          source={{ uri: `data:image/png;base64,${image}` }}
          style={styles.background}>
          {children}
        </FastImage>
      ) : (
        <LinearGradient
          colors={gradient}
          start={{ x: 0, y: 1 }}
          end={{ x: 1, y: 0 }}
          style={styles.background}>
          {children}
        </LinearGradient>
      ),
    [image]
  )

  return (
    <View style={{ flex: 1, paddingBottom: insets.bottom }}>
      <View style={styles.topContainer}>
        <Background>
          <View style={[styles.topInnerWrapper, { paddingTop: insets.top }]}>
            <Pressable
              style={Buttons.applyOpacity(styles.navigation)}
              onPress={onBackNavigationPress}
              hitSlop={10}>
              <LeftArrowIcon width={24} height={24} color={Colors.primary.s600} />
            </Pressable>
            <View style={styles.dateCard}>
              <Text style={styles.dateCardText}>
                {getEventCardDate(fromDate, toDate)}
              </Text>
            </View>
          </View>
          <View
            style={[
              styles.eventCardBodyWrapper,
              { paddingBottom: insets.bottom + Sizing.x15 },
            ]}>
            <Text style={[styles.eventTitle, { color: titleColor }]}>{title}</Text>
            <View
              style={[
                styles.shareButtonWrapper,
                { backgroundColor: applyOpacity(_color, 0.5) },
              ]}>
              <Pressable
                style={Buttons.applyOpacity(styles.shareButton)}
                onPress={onSharePress}>
                <ShareIcon
                  style={styles.icon}
                  strokeWidth={0.5}
                  stroke={Colors.primary.s800}
                  fill={Colors.primary.s800}
                />
              </Pressable>
            </View>
          </View>
        </Background>
      </View>
      <View
        style={[
          styles.bottomContainer,
          {
            backgroundColor: isLightMode ? Colors.primary.neutral : Colors.neutral.s600,
          },
        ]}>
        <KeyboardAwareScrollView style={styles.bottomWrapper}>
          {!isEventOwner ? (
            <View style={styles.sectionWrapper}>
              <UserIcon
                style={styles.icon}
                strokeWidth={1.8}
                stroke={isLightMode ? Colors.primary.s600 : Colors.primary.s200}
              />
              <SubHeaderText
                customStyle={{ marginBottom: Sizing.x5 }}
                colors={[Colors.primary.s800, Colors.primary.neutral]}>
                {organizerAlias}
              </SubHeaderText>
            </View>
          ) : (
            <></>
          )}
          <View style={styles.sectionWrapper}>
            <BodyText
              customStyle={{ fontFamily: "Roboto-Regular" }}
              changingColorScheme
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              {description ? description : "(no description)"}
            </BodyText>
          </View>
          <View style={styles.hourlyRateWrapper}>
            <SubHeaderText
              customStyle={styles.hourlyRateHeader}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              Hourly Rate:
            </SubHeaderText>
            <View style={styles.hourlyRateInnerWrapper}>
              <SubHeaderText
                key={"ada"}
                customStyle={styles.hourlyRateBody}
                colors={[Colors.primary.s800, Colors.primary.neutral]}>
                ADA: {eventHourlyRate().ada}
              </SubHeaderText>
              {eventHourlyRate().assets.map((asset) => (
                <SubHeaderText
                  key={asset[0]}
                  customStyle={styles.hourlyRateBody}
                  colors={[Colors.primary.s800, Colors.primary.neutral]}>
                  {hexToUtf8(asset[1].name)}: {asset[1].count}
                </SubHeaderText>
              ))}
            </View>
          </View>
        </KeyboardAwareScrollView>
        <View style={styles.buttonWrapper}>
          {isEventOwner && !bookedSlots?.length ? (
            <FullWidthButton
              onPressCallback={onDeleteEvent}
              text="Delete"
              colorScheme={colorScheme}
              textStyle={{ color: Colors.primary.neutral }}
              style={{
                backgroundColor: Colors.danger.s300,
                borderColor: Colors.danger.s300,
              }}
              loadingIndicator={isEventDeletionLoading}
            />
          ) : (
            <FullWidthButton
              onPressCallback={onBookEventPress}
              text="Pick a Date"
              colorScheme={colorScheme}
              loadingIndicator={isLoading}
              style={{ marginTop: "auto" }}
            />
          )}
        </View>
      </View>
    </View>
  )
}

const styles = StyleSheet.create({
  topContainer: {
    flex: 1,
  },
  bottomContainer: {
    flex: 1,
    alignItems: "center",
    overflow: "scroll",
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  bottomWrapper: {
    flex: 1,
    width: "90%",
    paddingVertical: Sizing.x20,
  },
  background: {
    width: "100%",
    height: "105%",
    position: "absolute",
    top: 0,
    flex: 1,
    alignItems: "center",
    justifyContent: "space-between",
    paddingBottom: Sizing.x15,
  },
  topInnerWrapper: {
    width: "90%",
    flexDirection: "row",
  },
  navigation: {
    borderRadius: Outlines.borderRadius.max,
    backgroundColor: Colors.primary.neutral,
    marginBottom: "auto",
    width: Sizing.x40,
    height: Sizing.x40,
    marginTop: Sizing.x15,
    alignItems: "center",
    justifyContent: "center",
  },
  dateCard: {
    maxWidth: Sizing.x85,
    height: "auto",
    marginLeft: "auto",
    marginTop: Sizing.x15,
    borderRadius: Outlines.borderRadius.small,
    backgroundColor: applyOpacity("000000", 0.3),
  },
  dateCardText: {
    textAlign: "center",
    padding: Sizing.x5,
    ...Typography.header.x40,
    color: Colors.primary.neutral,
    marginHorizontal: Sizing.x2,
  },
  eventCardBodyWrapper: {
    width: "90%",
    flexDirection: "row",
    alignItems: "flex-end",
    justifyContent: "space-between",
  },
  eventTitle: {
    alignSelf: "flex-start",
    maxWidth: "85%",
    marginTop: "auto",
    ...Typography.header.x60,
    color: Colors.primary.neutral,
  },
  shareButtonWrapper: {
    width: Sizing.x50,
    height: Sizing.x50,
    borderRadius: Outlines.borderRadius.max,
    alignItems: "center",
    justifyContent: "center",
  },
  shareButton: {
    width: Sizing.x40,
    height: Sizing.x40,
    borderRadius: Outlines.borderRadius.max,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
    justifyContent: "center",
  },
  sectionWrapper: {
    marginBottom: Sizing.x5,
    flexDirection: "row",
    alignItems: "center",
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
    marginRight: Sizing.x3,
  },
  buttonWrapper: {
    width: "90%",
    marginVertical: Sizing.x5,
  },
  hourlyRateWrapper: {},
  hourlyRateInnerWrapper: {},
  hourlyRateHeader: {
    ...Typography.header.x25,
  },
  hourlyRateBody: {},
})
