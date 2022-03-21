import * as React from "react";
import { View, Text, StyleSheet } from "react-native";

import PagerView from "react-native-pager-view";
import { LiveCollaborationIcon } from "icons/index";
import { LearnMoreModal } from "components/modals/learnMoreModal";
import { Colors, Typography, Sizing } from "styles/index";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { scale } from "lib/utils";

export interface InitialScreenProps {
  pagerRef: React.RefObject<PagerView>;
}

export const InitialScreen = ({ pagerRef }: InitialScreenProps) => {
  const [isVisibleModal, setIsVisibleModal] = React.useState<boolean>(false);

  const navigateToNextScreen = () => {
    pagerRef.current?.setPage(1);
  };

  const onPressLearnMore = () => {
    setIsVisibleModal(true);
  };

  return (
    <View style={styles.container}>
      <View style={styles.imageSection}>
        <LiveCollaborationIcon width="100%" height="100%" />
      </View>
      <View style={styles.main}>
        <Text style={styles.header}>Lorem ipsum dolor sit amet</Text>
        <View style={styles.subHeaderWrapper}>
          <Text style={styles.subHeader}>
            Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum
            venenatis quam sem, eget bibendum lorem convallis et. Donec velit
            ante, efficitur at ante eu, consequat hendrerit augue. Vivamus quis
            eros ex
          </Text>
        </View>
      </View>
      <View style={styles.buttons}>
        <FullWidthButton
          colorScheme="dark"
          onPressCallback={onPressLearnMore}
          text="Learn more"
        />
        <FullWidthButton
          colorScheme="dark"
          onPressCallback={navigateToNextScreen}
          buttonType="transparent"
          text="Next"
        />
      </View>
      <LearnMoreModal
        setIsVisibleModal={setIsVisibleModal}
        isVisibleModal={isVisibleModal}
      />
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    width: "90%",
    height: "100%",
  },
  imageSection: {
    flex: 2,
    marginTop: Sizing.x20,
    marginBottom: Sizing.x10,
  },
  main: {
    flex: 3,
    marginTop: Sizing.x20,
    justifyContent: "center",
    alignItems: "flex-start",
  },
  header: {
    ...Typography.header.x65,
    color: Colors.primary.neutral,
    marginBottom: scale(Sizing.x15),
  },
  subHeader: {
    ...Typography.body.x35,
    fontFamily: "Roboto-Regular",
    color: Colors.primary.neutral,
  },
  subHeaderWrapper: {
    marginBottom: Sizing.x20,
    alignItems: "center",
    justifyContent: "center",
  },
  buttons: {
    flex: 2,
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
  },
});
