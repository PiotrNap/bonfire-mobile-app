import * as React from "react"
import { View, Text, StyleSheet, useWindowDimensions, Pressable } from "react-native"

import Modal from "react-native-modal"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { BodyText } from "components/rnWrappers/bodyText"
import { appContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"

export interface BidSlideModalProps {
  children?: React.ReactNode
  isVisible: boolean
  hideModal: () => void
  icon?: JSX.Element
  header?: string
  body?: string
  buttonTitle?: string
  buttonCb?: () => void
  secondButtonTitle?: string
  secondButtonCb?: (arg?: any) => void
  buttonDisabled?: boolean
  secondButtonDisabled?: boolean
  customStyles?: any
}

export const BigSlideModal = ({
  isVisible,
  hideModal,
  header,
  body,
  buttonTitle,
  secondButtonTitle,
  buttonCb,
  secondButtonCb,
  children,
  buttonDisabled,
  secondButtonDisabled,
  customStyles,
}: BidSlideModalProps) => {
  const { colorScheme } = appContext()
  const [visible, setVisible] = React.useState<boolean>(isVisible)
  const windowWidth = useWindowDimensions().width
  const windowHeight = useWindowDimensions().height
  const isLightMode = colorScheme === "light"

  const onHideModal = () => setVisible(false)
  const onHidedModal = () => hideModal()

  return (
    <Modal
      onSwipeComplete={onHideModal}
      onBackButtonPress={onHideModal}
      onBackdropPress={onHideModal}
      onModalHide={onHidedModal}
      useNativeDriverForBackdrop
      useNativeDriver
      hideModalContentWhileAnimating
      swipeThreshold={100}
      swipeDirection={["down"]}
      deviceWidth={windowWidth}
      deviceHeight={windowHeight}
      isVisible={visible}
      style={[styles.modal, customStyles]}>
      <View
        style={[
          styles.container,
          isLightMode
            ? { backgroundColor: Colors.primary.neutral }
            : { backgroundColor: Colors.neutral.s600 },
        ]}>
        <View style={styles.main}>
          <Pressable
            hitSlop={Sizing.x20}
            style={[
              styles.topSwipeLine,
              isLightMode
                ? { backgroundColor: Colors.primary.s800 }
                : { backgroundColor: Colors.primary.neutral },
            ]}
            onPress={onHideModal}
          />
          <View style={styles.textContainer}>
            {header && (
              <Text
                style={isLightMode ? styles.headerText_light : styles.headerText_dark}>
                {header}
              </Text>
            )}
            {body && (
              <BodyText
                changingColorScheme
                colors={[Colors.primary.s600, Colors.primary.neutral]}>
                {body}
              </BodyText>
            )}
            {children}
          </View>
          <View style={styles.buttonContainer}>
            {buttonTitle && buttonCb && (
              <FullWidthButton
                onPressCallback={buttonCb}
                text={buttonTitle}
                colorScheme={colorScheme}
                disabled={buttonDisabled}
              />
            )}
            {secondButtonCb && secondButtonTitle && (
              <FullWidthButton
                onPressCallback={secondButtonCb}
                text={secondButtonTitle}
                colorScheme={colorScheme}
                disabled={secondButtonDisabled}
              />
            )}
          </View>
        </View>
      </View>
    </Modal>
  )
}

const styles = StyleSheet.create({
  modal: {
    flex: 1,
    height: "95%",
    width: "100%",
    margin: 0,
    position: "absolute",
    bottom: 0,
  },
  container: {
    height: "100%",
    alignItems: "center",
    backgroundColor: Colors.primary.neutral,
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  main: {
    flex: 1,
    width: "90%",
    alignItems: "center",
    marginTop: Sizing.x35,
  },
  topSwipeLine: {
    width: "40%",
    height: Sizing.x5,
    borderRadius: Outlines.borderRadius.max,
  },
  textContainer: {
    marginTop: Sizing.x10,
    width: "100%",
  },
  headerText_light: {
    ...Typography.header.x45,
    color: Colors.primary.s800,
    marginVertical: Sizing.x10,
  },
  headerText_dark: {
    ...Typography.header.x45,
    color: Colors.primary.neutral,
    marginVertical: Sizing.x10,
  },
  buttonContainer: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    marginBottom: Sizing.x40,
    marginTop: "auto",
  },
})
