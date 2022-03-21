import * as React from "react";
import { View, StyleSheet } from "react-native";

// import { AvailableTimeSlot } from "components/lists/availabilitiesScreen/AvailableTimeSlot";

export interface AvailableTimesListProps {
  availabilities: any;
}

export const AvailableTimesList = ({}: AvailableTimesListProps) => {
  // const renderItem = () => {
  //   return <AvailableTimeSlot time={100} />;
  // };

  // const keyExtractor = (item: any, index: number) => `${item.time}_${index}`;

  return (
    <View style={styles.container}>
      {/*  <FlatList
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
