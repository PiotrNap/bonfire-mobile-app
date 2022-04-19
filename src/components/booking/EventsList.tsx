import * as React from "react"
import {
  StyleSheet,
  ActivityIndicator,
  View,
  VirtualizedList,
} from "react-native"
import LottieView from "lottie-react-native"

import { EventsListCard } from "./EventsListCard"
import { bufferToBase64, getRandomKey } from "lib/utils"
import { useEventsPagination } from "lib/hooks/useEventsPagination"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Colors, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"

export interface EventsListProps {
  customEvents?: any[] | null
  customIsLoading?: boolean
  isOrganizerOwnEvents?: boolean
}

export const EventsList = ({
  customEvents,
  customIsLoading,
  isOrganizerOwnEvents,
}: EventsListProps) => {
  const { id } = React.useContext(ProfileContext)
  const { colorScheme, accountType } = appContext()
  const {
    events,
    isLoading: isPaginationLoading,
    getEventsPaginated,
    eventsPage,
  } = useEventsPagination()

  // we aren't showing organizers own events on browse-screen
  const filterOrganizerEvents = React.useCallback(
    (_events: any[]) => {
      return _events.filter((event) => event.organizerId !== id)
    },
    [customEvents, events, id]
  )
  const eventsList = () => {
    if (accountType === "attendee" || isOrganizerOwnEvents)
      return customEvents ?? events
    return filterOrganizerEvents(customEvents ?? events)
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
    } = item

    return (
      <EventsListCard
        title={title}
        titleColor={eventTitleColor}
        description={description}
        fromDate={fromDate}
        toDate={toDate}
        eventId={eventId}
        organizerId={organizerId}
        image={bufferToBase64(eventCardImage?.data)}
        color={eventCardColor}
        isTransparent={eventCardColor === "transparent"}
      />
    )
  }, [])

  const keyExtractor = (item: any, index: number) => getRandomKey(index)
  const getItem = (data: any, index: number) => data[index]
  const getItemCount = (data: any) => data.length
  const loadEvents = async (page: number, isRefreshing: boolean) => {
    await getEventsPaginated(page, isRefreshing)
  }

  const _ActivityIndicator = () => (
    <ActivityIndicator
      animating={true}
      color={isLightMode ? Colors.primary.s800 : Colors.primary.neutral}
      size="large"
      style={{ paddingTop: Sizing.x35 }}
    />
  )
  const onEndReach = () => loadEvents(eventsPage + 1, false)
  const onRefresh = React.useCallback(() => loadEvents(1, true), [])
  const onLayout = React.useCallback(
    ({ data, index }) => ({
      length: Sizing.x130,
      offset: Sizing.x130 * index,
      index,
    }),
    []
  )
  return (
    <>
      {!isEmptyEventsList && !isLoading ? (
        <VirtualizedList
          style={{ flex: 1, width: "100%" }}
          contentContainerStyle={{ width: "95%", alignSelf: "center" }}
          data={eventsList()}
          getItem={getItem}
          refreshing={isLoading}
          initialNumToRender={4}
          onEndReachedThreshold={0.5}
          onEndReached={onEndReach}
          getItemCount={getItemCount}
          renderItem={renderEventCard}
          keyExtractor={keyExtractor}
          onRefresh={onRefresh}
          onLayout={onLayout}
          showsVerticalScrollIndicator={false}
          ListFooterComponent={
            !customEvents && isLoading ? _ActivityIndicator : null
          }
          maxToRenderPerBatch={10}
          updateCellsBatchingPeriod={30}
          removeClippedSubviews
        />
      ) : isEmptyEventsList && isLoading ? (
        <_ActivityIndicator />
      ) : (
        <View style={styles.noEventsMessage}>
          <LottieView
            style={styles.lottieAnimation}
            source={require("../../../assets/animations/not-found.json")}
            autoPlay
            loop={false}
          />
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            It looks empty here...
          </SubHeaderText>
        </View>
      )}
    </>
  )
}

const styles = StyleSheet.create({
  noEventsMessage: {
    flex: 1,
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
  },
  lottieAnimation: {
    width: Sizing.x120,
    height: Sizing.x120,
  },
})
