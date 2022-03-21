import { appContext } from "contexts/contextApi";
import * as React from "react";
import { View, FlatList, Text, StyleSheet } from "react-native";

import { Colors, Sizing, Typography } from "styles/index";
import { HorizontalCardItem } from "./HorizontalCardItem";
import { HorizontalProfileCardItem } from "./HorizontalProfileCardItem";

export interface HorizontalCardsListProps {
  list: any;
  navigateTo: any;
}

export const HorizontalCardsList = ({
  list,
  navigateTo,
}: HorizontalCardsListProps) => {
  const { colorScheme } = appContext();

  const keyExtractor = (item: any, index: number) =>
    `${item.title}-${index}-${item.type}`;

  const renderItem = ({ item }: any) => {
    if (list.type === "categories") {
      return <HorizontalCardItem navigateTo={navigateTo} item={item} />;
    }
    if (list.type === "profiles") {
      return <HorizontalProfileCardItem navigateTo={navigateTo} item={item} />;
    }
    return <></>;
  };

  return (
    <View style={styles.container}>
      <View style={styles.header}>
        <Text
          style={
            colorScheme === "light"
              ? styles.headerTitle_light
              : styles.headerTitle_dark
          }>
          {list.title}
        </Text>
      </View>
      <View style={styles.listContainer}>
        <FlatList
          data={list.items}
          renderItem={renderItem}
          maxToRenderPerBatch={10}
          keyExtractor={keyExtractor}
          horizontal
          contentContainerStyle={{ paddingLeft: 20 }}
          showsHorizontalScrollIndicator={false}
        />
      </View>
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 1,
    alignItems: "center",
    marginVertical: Sizing.x20,
  },
  header: {
    alignSelf: "flex-start",
    marginLeft: Sizing.x35,
    marginBottom: Sizing.x14,
  },
  headerTitle_light: {
    ...Typography.header.x35,
    color: Colors.primary.s800,
  },
  headerTitle_dark: {
    ...Typography.header.x35,
    color: Colors.primary.neutral,
  },
  listContainer: {
    width: "100%",
    height: "100%",
    marginLeft: Sizing.x10,
  },
});
