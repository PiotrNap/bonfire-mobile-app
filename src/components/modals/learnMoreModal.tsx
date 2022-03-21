import * as React from "react";
import { View, Text, StyleSheet, Dimensions, Pressable } from "react-native";

import Modal from "react-native-modal";
import { IdeaIcon } from "icons/index";
import { Buttons, Colors, Typography, Sizing, Outlines } from "styles/index";
import { FullWidthButton } from "components/buttons/fullWidthButton";

export interface LearnMoreModalProps {
  setIsVisibleModal: (arg: boolean) => void;
  isVisibleModal: boolean;
}

export const LearnMoreModal = ({
  isVisibleModal,
  setIsVisibleModal,
}: LearnMoreModalProps) => {
  const { width, height } = Dimensions.get("screen");

  const onModalComplete = () => {
    setIsVisibleModal(false);
  };

  const onPress = () => {
    setIsVisibleModal(false);
  };

  return (
    <Modal
      isVisible={isVisibleModal}
      onSwipeComplete={onModalComplete}
      onBackButtonPress={onModalComplete}
      onBackdropPress={onModalComplete}
      useNativeDriverForBackdrop
      useNativeDriver
      hideModalContentWhileAnimating
      swipeThreshold={100}
      swipeDirection={["down"]}
      deviceHeight={height}
      deviceWidth={width}
      testID={"modal"}
      style={[styles.modalView, { width }]}>
      <View>
        <View style={styles.container}>
          <View style={styles.dragElemWrapper}>
            <View style={styles.dragElem} />
          </View>
          <View style={styles.mainWrapper}>
            <View style={styles.innerWrapper}>
              <View style={styles.iconBackground}>
                <IdeaIcon
                  width={Sizing.x50}
                  height={Sizing.x50}
                  stroke={Colors.primary.s800}
                />
              </View>
              <Text style={styles.header}>Lorem ipsum dolor sit amet</Text>
              <Text style={styles.subHeader}>
                Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                Vestibulum venenatis quam sem, eget bibendum lorem convallis et.
                Donec velit ante, efficitur at ante eu, consequat
              </Text>
            </View>
          </View>
          <View style={styles.buttonWrapper}>
            <FullWidthButton
              onPressCallback={onPress}
              text="Okay"
              colorScheme="light"
            />
          </View>
        </View>
      </View>
    </Modal>
  );
};

const styles = StyleSheet.create({
  modalView: {
    borderTopRightRadius: Sizing.x40,
    borderTopLeftRadius: Sizing.x40,
    maxHeight: "50%",
    top: "50%",
    margin: 0,
    backgroundColor: Colors.primary.neutral,
  },
  container: {
    width: "100%",
    height: "100%",
    alignItems: "center",
  },
  dragElemWrapper: {
    flex: 1,
    width: "100%",
    alignItems: "center",
  },
  dragElem: {
    width: "45%",
    marginTop: Sizing.x30,
    borderColor: Colors.primary.s300,
    borderRadius: Outlines.borderRadius.max,
    borderWidth: Sizing.x2,
  },
  mainWrapper: {
    flex: 6,
    width: "100%",
    height: "100%",
    justifyContent: "center",
  },
  innerWrapper: {
    width: "100%",
    height: "80%",
    justifyContent: "space-evenly",
    alignItems: "center",
  },
  iconBackground: {
    padding: 4,
    backgroundColor: Colors.primary.s400,
    borderRadius: Outlines.borderRadius.max,
  },
  textContainer: {
    alignItems: "center",
    width: "100%",
  },
  header: {
    ...Typography.header.x50,
    fontFamily: "Roboto-Medium",
    textAlign: "center",
    width: "90%",
    color: Colors.primary.s800,
    marginVertical: Sizing.x10,
  },
  subHeader: {
    ...Typography.subHeader.x30,
    textAlign: "center",
    width: "90%",
    color: Colors.primary.s800,
  },
  buttonWrapper: {
    flex: 2,
    marginVertical: Sizing.x10,
    alignItems: "center",
    width: "90%",
    height: "100%",
  },
});
