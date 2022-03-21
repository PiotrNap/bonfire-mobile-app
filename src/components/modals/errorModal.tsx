import * as React from "react";
import { View, StyleSheet, useWindowDimensions } from "react-native";

import { ErrorIcon } from "assets/icons";
import { Errors } from "common/types/errors";
import { SubHeaderText } from "components/rnWrappers/subHeaderText";
import { Colors, Outlines, Sizing } from "styles/index";
import Modal from "react-native-modal";

export interface ErrorModalProps {
  isModalVisible: boolean;
  errorType: string;
  errorHideCallback?: () => void;
}

export const ErrorModal = ({
  isModalVisible,
  errorHideCallback,
  errorType,
}: ErrorModalProps) => {
  const [isVisible, setIsVisible] = React.useState<boolean>(isModalVisible);
  const { width, height } = useWindowDimensions();

  React.useEffect(() => {
    if (isModalVisible) {
      setIsVisible(isModalVisible);
      let timeout = setTimeout(() => {
        setIsVisible(false);
        errorHideCallback && errorHideCallback();
      }, 7500);
      return () => clearTimeout(timeout);
    } else {
      setIsVisible(false);
    }
  }, [isModalVisible]);

  return (
    <Modal
      animationIn={"slideInDown"}
      animationInTiming={200}
      animationOut={"slideOutUp"}
      animationOutTiming={400}
      isVisible={isVisible}
      onSwipeCancel={() => setIsVisible(false)}
      deviceWidth={width}
      deviceHeight={height}
      hasBackdrop={false}
      coverScreen={false}
      swipeDirection="up"
      onSwipeComplete={() => setIsVisible(false)}
      style={styles.modal}>
      <View style={styles.main}>
        <ErrorIcon
          stroke={Colors.primary.neutral}
          width={Sizing.x60}
          height={Sizing.x60}
          strokeWidth={1.5}
        />
        <SubHeaderText customStyle={styles.text}>
          {Errors[`${errorType}`]}
        </SubHeaderText>
      </View>
    </Modal>
  );
};

const styles = StyleSheet.create({
  modal: {
    margin: 0,
  },
  main: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-around",
    height: Sizing.x80,
    width: "100%",
    marginBottom: "auto",
    backgroundColor: Colors.danger.s300,
    borderBottomLeftRadius: Outlines.borderRadius.base,
    borderBottomRightRadius: Outlines.borderRadius.base,
  },
  text: {
    flexWrap: "wrap",
    width: "80%",
  },
});
