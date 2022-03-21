import { getDay, getDigitalTime, getMonth } from "lib/utils";
import * as React from "react";

export const useRefreshControl = () => {
  const [refreshing, setRefreshing] = React.useState<boolean>(false);
  const [lastUpdated, setLastUpdated] = React.useState<number>(
    new Date().getTime()
  );

  // fetch new events, update calendar state
  const onRefresh = React.useCallback(async () => {
    const wait = (ms: number): Promise<void> => {
      return new Promise((res) => setTimeout(res, ms));
    };

    setRefreshing(true);
    await wait(2500);
    setRefreshing(false);
    setLastUpdated(new Date().getTime());
  }, [refreshing]);

  // @TODO: Store lastUpdated value inside context store for future reference
  const lastUpdatedDay =
    getDay(lastUpdated) - getDay() === -1
      ? `Yestarday ${getDigitalTime(lastUpdated)}`
      : getDay(lastUpdated) - getDay() < -1
      ? `${getDay(lastUpdated)}/${getMonth(lastUpdated)}`
      : `Today ${getDigitalTime(lastUpdated)}`;

  const title = `Last updated: ${lastUpdatedDay}`;

  return {
    refreshing,
    lastUpdated,
    title,
    onRefresh,
  };
};
