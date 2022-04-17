import * as React from "react"
import { View, StyleSheet, useWindowDimensions, TextStyle } from "react-native"

import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Outlines, Sizing, Typography } from "styles/index"
import Modal from "react-native-modal"

export interface SlideTopModalProps {
  isModalVisible: boolean
  icon: any
  backgroundColor: string
  modalContent: string
  contentStyle?: TextStyle
  hideCallback?: () => void
}

export const SlideTopModal = ({
  isModalVisible,
  hideCallback,
  modalContent,
  backgroundColor,
  contentStyle,
  icon,
}: SlideTopModalProps) => {
  const [isVisible, setIsVisible] = React.useState<boolean>(isModalVisible)
  const { width, height } = useWindowDimensions()

  React.useEffect(() => {
    if (isModalVisible) {
      setIsVisible(isModalVisible)
      let timeout = setTimeout(() => {
        setIsVisible(false)
        hideCallback && hideCallback()
      }, 2500)
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
      <View style={[styles.main, { backgroundColor }]}>
        {icon}
        <SubHeaderText customStyle={[styles.text, contentStyle]}>
          {modalContent}
        </SubHeaderText>
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
    borderBottomLeftRadius: Outlines.borderRadius.base,
    borderBottomRightRadius: Outlines.borderRadius.base,
  },
  text: {
    flexWrap: "wrap",
    width: "80%",
    ...Typography.fontWeight.semibold,
  },
})
