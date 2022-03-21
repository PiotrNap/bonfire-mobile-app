import * as React from "react";
import { View, StyleSheet, FlatList, ListRenderItemInfo } from "react-native";

import { eventCreationContext } from "contexts/contextApi";
import { getRandomKey } from "lib/utils";
import { Availability } from "./availability";

export interface AvailabilityListProps {}

export const AvailabilityList = ({}: AvailabilityListProps) => {
  const { availabilities, removeAvailability } = eventCreationContext();

  const renderAvailabilities = ({ item, index }: ListRenderItemInfo<any>) => {
    if (item) {
      return (
        <Availability
          availability={item}
          index={index}
          onRemovePress={onRemovePress}
        />
      );
    } else {
      return <></>;
    }
  };

  const keyExtractor = (item: any, index: number) => `${getRandomKey(index)}`;
  const onRemovePress = (index: number) =>
    removeAvailability(availabilities[index]);

  return (
    <View style={styles.container}>
      <FlatList
        data={availabilities}
        renderItem={renderAvailabilities}
        keyExtractor={keyExtractor}
        style={{ marginVertical: 8 }}
      />
    </View>
  );
};

const styles = StyleSheet.create({
  container: { flex: 1, width: "100%" },
});
