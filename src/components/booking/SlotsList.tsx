import * as React from "react"
import { StyleSheet, ActivityIndicator, View, VirtualizedList, Text } from "react-native"

import { getRandomKey } from "lib/utils"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Colors, Sizing, Typography } from "styles/index"
import { appContext } from "contexts/contextApi"
import { ProfileContext } from "contexts/profileContext"
import { CurvedArrow, PlusIcon } from "assets/icons"
import { RoundedButton } from "components/buttons/roundedButton"
import { useNavigation } from "@react-navigation/native"
import { showErrorToast } from "lib/helpers"
import { Events } from "Api/Events"
import { EventBookingSlot, PAGINATED_RESULTS_COUNT } from "common/types/dto"
import { SlotsListItem } from "./SlotsListItem"

type SlotsListProps = {
  listType: "bookedSlots" | "scheduledSlots"
}

export const SlotsList = ({ listType }: SlotsListProps) => {
  const { id } = React.useContext(ProfileContext)
  const { colorScheme, accountType } = appContext()
  const [isLoading, setIsLoading] = React.useState<boolean>(false)
  const [slots, setSlots] = React.useState<EventBookingSlot[]>([])
  const [page, setPage] = React.useState<number>(1)
  const [isLastPage, setIsLastPage] = React.useState<boolean>(false)
  const navigation = useNavigation()

  const fetchSlots = async (refresh = false) => {
    if (!id) return showErrorToast("", "Unable to fetch Events")
    setIsLoading(true)
    try {
      let res

      if (listType === "bookedSlots") {
        res = await Events.getBookingsByQuery({
          limit: 20,
          page: refresh ? 1 : page,
          attendee_id: id,
        })
      } else if (listType === "scheduledSlots") {
        res = await Events.getBookingsByQuery({
          limit: 20,
          page: refresh ? 1 : page,
          organizer_id: id,
        })
      } else throw new Error("Unknown type of slots to fetch")

      if (!res) throw new Error("Unable to fetch booked slots")

      const [paginatedSlots, count] = res
      console.log(paginatedSlots, count)
      if (count < PAGINATED_RESULTS_COUNT || count === 0) {
        setIsLastPage(true)
      }
      setSlots(paginatedSlots)
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
      setPage(refresh ? 1 : (prev) => prev + 1)
    }
  }

  React.useEffect(() => {
    fetchSlots()
  }, [])

  const isLightMode = colorScheme !== "dark"
  const isEmptyList = slots.length < 1

  const renderEventCard = React.useCallback(({ item, index }: any) => {
    return <SlotsListItem item={item} slotType={listType} />
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
  const onAddEventPress = () => {
    navigation.navigate("New Event Description")
  }
  const onEndReach = () => {
    if (isLastPage) return
    fetchSlots(false)
  }
  const onRefresh = () => fetchSlots(true)
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
      {!isEmptyList && !isLoading ? (
        <>
          <VirtualizedList
            style={{ flex: 1, width: "100%" }}
            contentContainerStyle={{ width: "95%", alignSelf: "center" }}
            data={slots}
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
          <View style={styles.cornerButtonWrapper}>
            <RoundedButton
              onPress={onAddEventPress}
              icon={
                <PlusIcon
                  color={isLightMode ? Colors.primary.neutral : Colors.primary.s800}
                  width={Sizing.x40}
                  height={Sizing.x40}
                  strokeWidth={2}
                />
              }
            />
          </View>
        </>
      ) : isLoading ? (
        <_ActivityIndicator />
      ) : (
        <View style={styles.noEventsMessage}>
          {false ? (
            //@TODO change this to something else
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
          <View style={styles.cornerButtonWrapper}>
            <RoundedButton
              onPress={onAddEventPress}
              icon={
                <PlusIcon
                  color={isLightMode ? Colors.primary.neutral : Colors.primary.s800}
                  width={Sizing.x40}
                  height={Sizing.x40}
                  strokeWidth={2}
                />
              }
            />
          </View>
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
    ...Typography.subHeader.x25,
  },
  cornerButtonWrapper: {
    position: "absolute",
    bottom: Sizing.x15,
    right: Sizing.x15,
  },
})
