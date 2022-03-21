import * as React from "react";
import {
  View,
  StyleSheet,
  FlatList,
  RefreshControl,
  FlatListProps,
  ActivityIndicator,
  Text,
} from "react-native";

import { appContext } from "contexts/contextApi";
import { getDigitalTime, getTime } from "lib/utils";
import { Colors, Sizing, Typography } from "styles/index";

import { TransactionItem } from "./transactionItem";
import { transactions } from "../../api_data/transactions";
import { ProfileContext } from "contexts/profileContext";

function wait(ms: number): Promise<void> {
  return new Promise((res) => setTimeout(res, ms));
}

export interface TransactionListProps {
  isSmallScreen: boolean;
  isLoading: boolean;
}

export const TransactionsList = ({
  isLoading,
  isSmallScreen,
}: TransactionListProps) => {
  const { colorScheme } = appContext();
  const { hasSyncedWallet } = React.useContext(ProfileContext);
  const [refreshing, setRefreshing] = React.useState<boolean>(false);
  const [lastRefreshed, setLastRefreshed] = React.useState<number>(0);
  const isLightMode = colorScheme === "light";

  const onRefresh = React.useCallback(() => {
    setRefreshing(true);
    wait(2000).then(() => {
      setRefreshing(false);
      setLastRefreshed(getTime());
    });
  }, []);

  const renderItem = ({ item }: any) => <TransactionItem item={item} />;

  const refreshControlTitle = `Last updated: ${getDigitalTime(lastRefreshed)}`;

  // @TODO needs more scalable solution
  const keyExtractor = ({ date, withUser }: any) => `${date}-${withUser}`;

  var flatListProps: FlatListProps<any> = {
    data: transactions,
    renderItem: renderItem,
    keyExtractor: keyExtractor,
    contentOffset: { x: 0, y: -25 },
  };

  if (!isSmallScreen) {
    flatListProps.refreshControl = (
      <RefreshControl
        title={lastRefreshed ? refreshControlTitle : ""}
        tintColor={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
        refreshing={refreshing}
        onRefresh={onRefresh}
      />
    );
  }

  return (
    <View style={[styles.container, isLoading ? { opacity: 0.5 } : {}]}>
      {hasSyncedWallet ? (
        <>
          <FlatList
            {...flatListProps}
            scrollEnabled={!isLoading}
            disableScrollViewPanResponder={isLoading}
          />
          {isLoading && (
            <ActivityIndicator
              size={isSmallScreen ? "small" : "large"}
              color={Colors.primary.s800}
              style={styles.spinner}
            />
          )}
        </>
      ) : (
        <Text
          style={[
            styles.innerHeader,
            {
              color: isLightMode ? Colors.primary.s800 : Colors.primary.neutral,
            },
          ]}>
          Please connect wallet to see your transactions.
        </Text>
      )}
    </View>
  );
};

const styles = StyleSheet.create({
  container: { flex: 1, marginTop: 10 },
  spinner: {
    position: "absolute",
    top: "50%",
    left: "50%",
  },
  innerHeader: {
    alignSelf: "center",
    marginTop: Sizing.x10,
    ...Typography.subHeader.x20,
  },
});
