import * as React from "react";
import { View, Text, StyleSheet, FlatList } from "react-native";
// import { AvailableTimeSlot } from "./AvailableTimeSlot";

export interface AvailableTimesListProps {
  availabilities: any;
}

export const AvailableTimesList = ({
  availabilities,
}: AvailableTimesListProps) => {
  // const renderItem = ({ item, index }) => {
  //   return <AvailableTimeSlot time={item.time} />;
  // };

  // const keyExtractor = (item, index) => `${item.time}_${index}`;

  return (
    <View style={styles.container}>
      {/*<FlatList
        renderItem={renderItem}
        data={availabilities}
        keyExtractor={keyExtractor}
      />*/}
    </View>
  );
};

const styles = StyleSheet.create({
  container: { flex: 1 },
});
