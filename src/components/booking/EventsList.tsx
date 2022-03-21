import * as React from "react";
import {
  StyleSheet,
  ActivityIndicator,
  View,
  VirtualizedList,
} from "react-native";

import { EventsListCard } from "./EventsListCard";
import { getRandomKey } from "lib/utils";
import { useEventsPagination } from "lib/hooks/useEventsPagination";
import { SubHeaderText } from "components/rnWrappers/subHeaderText";
import { Colors, Sizing } from "styles/index";
import { appContext } from "contexts/contextApi";

export interface EventsListProps {
  customEvents?: any[] | null;
}

export const EventsList = ({ customEvents }: EventsListProps) => {
  const { events, isLoading, getEventsPaginated, eventsPage } =
    useEventsPagination();
  const { colorScheme } = appContext();
  const isLightMode = colorScheme !== "dark";

  const renderEventCard = React.useCallback(({ item }: any) => {
    const {
      title,
      eventTitleColor,
      description,
      fromDate,
      toDate,
      // selectedDays,
      imageURI,
      eventCardColor,
      id,
      organizerId,
    } = item;
    // const selectedDaysArr: number[] = Object.values(selectedDays ?? {});
    // const fromDate = Math.min(...selectedDaysArr);
    // const toDate = Math.max(...selectedDaysArr);

    return (
      <EventsListCard
        title={title}
        titleColor={eventTitleColor}
        description={description}
        fromDate={fromDate}
        toDate={toDate}
        id={id}
        organizerId={organizerId}
        image={imageURI}
        color={eventCardColor}
        isTransparent={eventCardColor === "transparent"}
      />
    );
  }, []);

  const keyExtractor = (item: any, index: number) => getRandomKey(index);
  const getItem = (data: any, index: number) => data[index];
  const getItemCount = (data: any) => data.length;
  const loadEvents = async (page: number, isRefreshing: boolean) => {
    await getEventsPaginated(page, isRefreshing);
  };

  const _ActivityIndicator = () => (
    <ActivityIndicator
      animating={true}
      color={isLightMode ? Colors.primary.s800 : Colors.primary.neutral}
      size="large"
      style={{ paddingTop: Sizing.x35 }}
    />
  );
  const onEndReach = () => loadEvents(eventsPage + 1, false);
  const onRefresh = React.useCallback(() => loadEvents(1, true), []);
  const onLayout = React.useCallback(
    ({ data, index }) => ({
      length: Sizing.x130,
      offset: Sizing.x130 * index,
      index,
    }),
    []
  );

  return (
    <>
      {!!events.length && !isLoading ? (
        <VirtualizedList
          style={{ flex: 1, width: "100%" }}
          contentContainerStyle={{ width: "95%", alignSelf: "center" }}
          data={customEvents ?? events}
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
      ) : !events.length && isLoading ? (
        <_ActivityIndicator />
      ) : (
        <View style={styles.noEventsMessage}>
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            Nothing yet to show...
          </SubHeaderText>
        </View>
      )}
    </>
  );
};

const styles = StyleSheet.create({
  noEventsMessage: {
    flex: 1,
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
  },
});
