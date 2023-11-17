import * as React from "react"
import { StyleSheet, ActivityIndicator, View, VirtualizedList } from "react-native"
import LottieView from "lottie-react-native"

import { EventsListCard } from "./EventsListCard"
import { bufferToBase64, getRandomKey } from "lib/utils"
import { useEventsPagination } from "lib/hooks/useEventsPagination"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Colors, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { CurvedArrow } from "assets/icons"

export const EventsList = React.forwardRef((props, ref): any => {
  const { customEvents, customIsLoading, isOrganizerOwnEvents } = props
  const { id } = React.useContext(ProfileContext)
  const { colorScheme, accountType } = appContext()
  const {
    events,
    isLoading: isPaginationLoading,
    isLastPage,
    getEventsPaginated,
    eventsPage,
  } = useEventsPagination(isOrganizerOwnEvents ? id : "")

  React.useImperativeHandle(ref, () => ({
    getEventsPaginated,
  }))

  // we aren't showing organizers own events on browse-screen
  const filterUserEvents = React.useCallback(
    (_events: any[]) => {
      return _events.filter((event) => event.organizerId !== id)
    },
    [customEvents, events, id]
  )
  const eventsList = () => {
    if (accountType === "attendee" || isOrganizerOwnEvents) return customEvents ?? events
    return filterUserEvents(customEvents ?? events)
  }
  const isLightMode = colorScheme !== "dark"
  const isEmptyEventsList = !eventsList().length
  const isLoading = customIsLoading || isPaginationLoading

  const renderEventCard = React.useCallback(({ item }: any) => {
    const {
      title,
      eventTitleColor,
      description,
      fromDate,
      toDate,
      eventCardImage,
      eventCardColor,
      id: eventId,
      organizerId,
      organizerAlias,
      hourlyRate,
    } = item

    return (
      <EventsListCard
        title={title}
        titleColor={eventTitleColor}
        description={description}
        fromDate={fromDate}
        toDate={toDate}
        eventId={eventId}
        organizerAlias={organizerAlias}
        organizerId={organizerId}
        image={bufferToBase64(eventCardImage?.data)}
        color={eventCardColor}
        isTransparent={eventCardColor === "transparent"}
        hourlyRate={hourlyRate}
      />
    )
  }, [])

  const keyExtractor = () => getRandomKey(5)
  const getItem = (data: any, index: number) => data[index]
  const getItemCount = (data: any) => data.length

  const _ActivityIndicator = () => (
    <ActivityIndicator
      animating={true}
      color={isLightMode ? Colors.primary.s800 : Colors.primary.neutral}
      size="large"
      style={{ paddingTop: Sizing.x35 }}
    />
  )
  const onEndReach = React.useCallback(() => {
    if (isLastPage) return
    getEventsPaginated(eventsPage + 1, 20, id)
  }, [isLastPage])
  const onRefresh = () => getEventsPaginated(1, 20, id, true)
  const onLayout = React.useCallback(
    ({ data, index }) => ({
      length: Sizing.x130,
      offset: Sizing.x130 * index,
      index,
    }),
    []
  )
  //@TODO fix loading indicator at the bottom
  return (
    <>
      {!isEmptyEventsList && !isLoading ? (
        <VirtualizedList
          style={{ flex: 1, width: "100%" }}
          contentContainerStyle={{ width: "95%", alignSelf: "center" }}
          data={eventsList()}
          getItem={getItem}
          refreshing={isLoading}
          initialNumToRender={5}
          onEndReachedThreshold={0.7}
          onEndReached={onEndReach}
          getItemCount={getItemCount}
          renderItem={renderEventCard}
          keyExtractor={keyExtractor}
          onRefresh={onRefresh}
          onLayout={onLayout}
          showsVerticalScrollIndicator={false}
          ListFooterComponent={isLoading ? _ActivityIndicator : null}
          maxToRenderPerBatch={10}
          updateCellsBatchingPeriod={30}
          removeClippedSubviews
        />
      ) : isLoading ? (
        <_ActivityIndicator />
      ) : (
        <View style={styles.noEventsMessage}>
          {isOrganizerOwnEvents ? (
            <SubHeaderText
              customStyle={styles.noEventsText}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              Your event list is empty right now. Start adding events by tapping the '+'
              button and watch this space fill up with your plans!
            </SubHeaderText>
          ) : (
            <SubHeaderText
              customStyle={styles.noEventsText}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              Looks like there are no events to show right now. Be the first to create an
              event and inspire the community!
            </SubHeaderText>
          )}
          <CurvedArrow
            style={[
              styles.arrowIcon,
              //@ts-ignore
              {
                fill: isLightMode ? Colors.primary.s350 : Colors.primary.s350,
                transform: [{ rotate: "90deg" }],
              },
            ]}
          />
        </View>
      )}
    </>
  )
})

const styles = StyleSheet.create({
  noEventsMessage: {
    flex: 1,
    width: "100%",
    alignItems: "center",
    justifyContent: "flex-end",
    paddingBottom: Sizing.x60,
  },
  lottieAnimation: {
    width: Sizing.x120,
    height: Sizing.x120,
  },
  arrowIcon: {
    width: Sizing.x60,
    height: Sizing.x60,
    alignSelf: "flex-end",
    marginRight: Sizing.x80,
    marginTop: Sizing.x20,
  },
  noEventsText: {
    textAlign: "center",
  },
})
