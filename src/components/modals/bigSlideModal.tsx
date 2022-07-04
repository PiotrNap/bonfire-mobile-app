import * as React from "react"
import {
  View,
  Text,
  StyleSheet,
  useWindowDimensions,
  Pressable,
} from "react-native"

import Modal from "react-native-modal"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { BodyText } from "components/rnWrappers/bodyText"
import { appContext } from "contexts/contextApi"
import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { ProfileContext } from "contexts/profileContext"

export interface BidSlideModalProps {
  children?: React.ReactNode
  isVisible: boolean
  hideModal: () => void
  icon?: JSX.Element
  header: string
  body?: string
  buttonTitle: string
  buttonCb: () => void
  secondButtonTitle?: string
  secondButtonCb?: () => void
}

export const BigSlideModal = ({
  isVisible,
  hideModal,
  icon,
  header,
  body,
  buttonTitle,
  secondButtonTitle,
  buttonCb,
  secondButtonCb,
  children,
}: BidSlideModalProps) => {
  const { setHasSyncedWallet } = React.useContext(ProfileContext)
  const { colorScheme } = appContext()
  const [visible, setVisible] = React.useState<boolean>(isVisible)
  const windowWidth = useWindowDimensions().width
  const windowHeight = useWindowDimensions().height
  const isLightMode = colorScheme === "light"

  const onHideModal = () => setVisible(false)
  const onHidedModal = () => hideModal()

  const onLinkWallet = () => {
    setHasSyncedWallet(true)
    setVisible(false)
  }

  return (
    <View>
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
        style={[styles.modal, { width: windowWidth }]}>
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
            {/*
             // icons are not visible in dark mode
            <View style={{ backgroundColor: Colors.primary.neutral }}>
              {icon}
            </View>
            */}
            <View style={styles.textContainer}>
              <Text
                style={
                  isLightMode ? styles.headerText_light : styles.headerText_dark
                }>
                {header}
              </Text>
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
              <FullWidthButton
                onPressCallback={buttonCb}
                text={buttonTitle}
                colorScheme={colorScheme}
              />
              {secondButtonCb && secondButtonTitle && (
                <FullWidthButton
                  onPressCallback={secondButtonCb}
                  text={secondButtonTitle}
                  colorScheme={colorScheme}
                />
              )}
            </View>
          </View>
        </View>
      </Modal>
    </View>
  )
}

const styles = StyleSheet.create({
  modal: {
    flex: 1,
    height: "95%",
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
    marginTop: "auto",
    marginBottom: "auto",
  },
  headerText_light: {
    ...Typography.header.x55,
    color: Colors.primary.s800,
    marginVertical: Sizing.x20,
  },
  headerText_dark: {
    ...Typography.header.x55,
    color: Colors.primary.neutral,
    marginVertical: Sizing.x20,
  },
  buttonContainer: {
    width: "100%",
    alignItems: "center",
    justifyContent: "center",
    marginBottom: Sizing.x40,
    marginTop: "auto",
  },
})
