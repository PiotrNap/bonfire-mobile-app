import * as React from "react"
import { StyleSheet, ActivityIndicator, View, VirtualizedList } from "react-native"

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
  reload: boolean
}

export const SlotsList = ({ listType, reload }: SlotsListProps) => {
  const { id } = React.useContext(ProfileContext)
  const { colorScheme, networkId } = appContext()
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
          network_id: networkId,
        })
      } else if (listType === "scheduledSlots") {
        res = await Events.getBookingsByQuery({
          limit: 20,
          page: refresh ? 1 : page,
          organizer_id: id,
          network_id: networkId,
        })
      } else throw new Error("Unknown type of slots to fetch")

      if (!res) throw new Error("Unable to fetch booked slots")

      const [paginatedSlots, count] = res

      if (count < PAGINATED_RESULTS_COUNT || count === 0) {
        setIsLastPage(true)
      }
      setSlots(paginatedSlots)
    } catch (e) {
      showErrorToast(e)
    } finally {
      setIsLoading(false)
      setPage(refresh ? 1 : (prev) => Number(prev) + 1)
    }
  }

  React.useLayoutEffect(() => {
    fetchSlots(true)
  }, [reload, networkId])

  const isLightMode = colorScheme !== "dark"
  const isEmptyList = slots.length < 1

  const renderEventCard = React.useCallback(
    ({ item, index }: any) => {
      return <SlotsListItem item={item} slotType={listType} />
    },
    [slots]
  )

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
    fetchSlots()
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
      ) : listType === "bookedSlots" ? (
        <View style={styles.noBookingsMessage}>
          {slots.length < 1 ? (
            <SubHeaderText
              customStyle={styles.noEventsText}
              colors={[Colors.primary.s800, Colors.primary.neutral]}>
              You don't have any booked events.{"\n"}Explore what the community has to
              offer!
            </SubHeaderText>
          ) : (
            <></>
          )}
          <CurvedArrow
            style={[
              styles.bookingsArrowIcon,
              //@ts-ignore
              {
                fill: isLightMode ? Colors.primary.s350 : Colors.primary.s350,
                transform: [{ rotate: "150deg" }],
              },
            ]}
          />
        </View>
      ) : (
        <View style={styles.noSchedulingsMessage}>
          <SubHeaderText
            customStyle={styles.noEventsText}
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            You don't have any scheduled meetings.{"\n"}Create new event and let people
            discover you!
          </SubHeaderText>
          <CurvedArrow
            style={[
              styles.schedulingsArrowIcon,
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
  noBookingsMessage: {
    flex: 1,
    width: "100%",
    alignItems: "center",
    justifyContent: "flex-end",
    paddingBottom: Sizing.x15,
  },
  noSchedulingsMessage: {
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
  bookingsArrowIcon: {
    width: Sizing.x60,
    height: Sizing.x60,
    alignSelf: "flex-start",
    marginLeft: Sizing.x80,
    marginTop: Sizing.x40,
  },
  schedulingsArrowIcon: {
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
