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
import {
  appContext,
  bookingContext,
  myCalendarContext,
} from "contexts/contextApi"
import { BodyText } from "components/rnWrappers/bodyText"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import tinyColor from "tinycolor2"
import { Events } from "Api/Events"
import { ProfileContext } from "contexts/profileContext"
import { EventStatistics } from "components/events/eventDescription/EventStatistics"
import dayjs from "dayjs"
import FastImage from "react-native-fast-image"
import { months } from "common/types/calendarTypes"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { shareEvent } from "lib/helpers"
import { Availabilities } from "common/interfaces/myCalendarInterface"

export const EventDescription = ({ navigation, route }: any) => {
  const {
    title,
    description,
    eventId,
    organizerId,
    organizerAlias,
    fromDate,
    toDate,
    image,
    color,
    titleColor,
    numOfBookedSlots,
  } = route.params
  const { colorScheme } = appContext()
  const { setPreviewingEvent, resetBookingState } = bookingContext()
  const { setAvailCalendar, loadMyCalendar, changeMonthHeader } =
    myCalendarContext()
  const { id } = React.useContext(ProfileContext)

  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [isEventOwner, setIsEventOwner] = React.useState<boolean>(false)

  const insets = useSafeAreaInsets()
  const isLightMode = colorScheme === "light"
  const _color = tinyColor(color).toHexString()

  const onBackNavigationPress = () => navigation.goBack()
  const onBookEventPress = async () => {
    setIsLoading(true)

    try {
      let event

      // We have all event info from previous fetch when navigating with deep link
      if (route.params?.fromShareLink) {
        const { fromShareLink, ...rest } = route.params
        event = rest
      } else {
        event = await Events.getEventById(eventId)
      }

      if (!event) return

      //TODO let organizers decide what's the 'good until' booking window period
      const availableDays: Availabilities[] = convertToCalendarAvailabilities(
        event.selectedDays,
        event.availabilities
      )
      resetBookingState()

      if (!availableDaysLeftInCurrMonth(Object.values(event.selectedDays))) {
        const nextAvailableMonth: number | undefined = (
          Object.values(event.selectedDays) as number[]
        ).find(
          (d: any) =>
            dayjs(d).month() > dayjs().month() &&
            dayjs(d).year() >= dayjs().year()
        )
        if (!nextAvailableMonth) return

        const availableStartDate = dayjs(nextAvailableMonth)
        const calendarSetup = {
          nextMonths: true,
          month: availableStartDate.month(),
          year: availableStartDate.year(),
          isBookingCalendar: true,
          isRegularCalendar: false,
          availabilities: availableDays,
          startFromCustomMonth: true,
          isNewCalendar: true,
        }
        loadMyCalendar(calendarSetup)
        changeMonthHeader({
          month: months[availableStartDate.month()],
          year: availableStartDate.year(),
          numOfEvents: 0,
          startingDate: new Date(
            availableStartDate.year(),
            availableStartDate.month()
          ),
        })
      } else {
        setAvailCalendar(availableDays)
      }
      setPreviewingEvent(Object.assign({}, event, route.params))
      setIsLoading(false)

      navigation.navigate("Available Event Days Selection", {
        ...route.params,
      })
    } catch (e) {
      // TODO Implement better error handling *error modal?*
      console.error(e.response)
      setIsLoading(false)
    }
  }

  const onEventDetailPreview = () =>
    navigation.navigate("Event Details", {
      header: "Details",
      organizerEvent: { ...route.params },
    })
  const onSharePress = async () => await shareEvent(eventId)

  React.useEffect(() => {
    if (id && organizerId && id === organizerId) setIsEventOwner(true)
  }, [])

  return (
    <View style={{ flex: 1, paddingBottom: insets.bottom }}>
      <View style={styles.topContainer}>
        <FastImage
          source={{ uri: `data:image/png;base64,${image}` }}
          style={styles.backgroundImage}>
          <View
            style={[
              styles.topInnerContainer,
              { backgroundColor: !image ? Colors.primary.s600 : color },
            ]}>
            <View style={[styles.topInnerWrapper, { paddingTop: insets.top }]}>
              <Pressable
                style={Buttons.applyOpacity(styles.navigation)}
                onPress={onBackNavigationPress}
                hitSlop={10}>
                <LeftArrowIcon
                  width={24}
                  height={24}
                  color={Colors.primary.s600}
                />
              </Pressable>
              <View
                style={[
                  styles.dateCard,
                  { backgroundColor: applyOpacity(_color, 0.5) },
                ]}>
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
              <Text style={[styles.eventTitle, { color: titleColor }]}>
                {title}
              </Text>
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
          </View>
        </FastImage>
      </View>
      <View
        style={[
          styles.bottomContainer,
          {
            backgroundColor: isLightMode
              ? Colors.primary.neutral
              : Colors.neutral.s600,
          },
        ]}>
        <View style={styles.bottomWrapper}>
          {!isEventOwner ? (
            <View style={styles.eventOrganizer}>
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
          <BodyText
            customStyle={{ fontFamily: "Roboto-Regular" }}
            changingColorScheme
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            {description}
          </BodyText>
          {isEventOwner ? (
            <View style={{ marginTop: "auto" }}>
              <EventStatistics
                views={0}
                bookings={numOfBookedSlots ?? 0}
                likes={0}
              />
              <FullWidthButton
                onPressCallback={onEventDetailPreview}
                text="Preview"
                colorScheme={colorScheme}
                loadingIndicator={isLoading}
              />
            </View>
          ) : (
            <FullWidthButton
              onPressCallback={onBookEventPress}
              text="Book Event"
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
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  bottomWrapper: {
    flex: 1,
    width: "90%",
    paddingVertical: Sizing.x20,
    // justifyContent: "space-between",
  },
  backgroundImage: {
    width: "100%",
    height: "105%",
    position: "absolute",
    top: 0,
  },
  topInnerContainer: {
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
  },
  dateCardText: {
    textAlign: "center",
    padding: Sizing.x5,
    ...Typography.header.x45,
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
  eventOrganizer: {
    marginBottom: Sizing.x5,
    flexDirection: "row",
    alignItems: "center",
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
    marginRight: Sizing.x3,
  },
})
