import * as React from "react"
import { View, StyleSheet, useWindowDimensions } from "react-native"

import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Colors, Outlines, Sizing } from "styles/index"
import Modal from "react-native-modal"

export interface SlideTopModalProps {
  isModalVisible: boolean
  icon: any
  modalContent: string
  errorHideCallback?: () => void
}

export const SlideTopModal = ({
  isModalVisible,
  errorHideCallback,
  modalContent,
  icon,
}: SlideTopModalProps) => {
  const [isVisible, setIsVisible] = React.useState<boolean>(isModalVisible)
  const { width, height } = useWindowDimensions()

  React.useEffect(() => {
    if (isModalVisible) {
      setIsVisible(isModalVisible)
      let timeout = setTimeout(() => {
        setIsVisible(false)
        errorHideCallback && errorHideCallback()
      }, 7500)
      return () => clearTimeout(timeout)
    } else {
      setIsVisible(false)
    }
  }, [isModalVisible])

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
        {icon}
        <SubHeaderText customStyle={styles.text}>{modalContent}</SubHeaderText>
      </View>
    </Modal>
  )
}

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
})
