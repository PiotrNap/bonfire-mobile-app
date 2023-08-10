import * as React from "react"
import { View, Text, StyleSheet, Dimensions } from "react-native"

import Modal from "react-native-modal"
import { IdeaIcon } from "icons/index"
import { Colors, Typography, Sizing, Outlines } from "styles/index"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { useNavigation } from "@react-navigation/native"

export interface SlideDownModalProps {
  setModalState: (ms: ModalState) => void
  isVisibleModal: boolean
  modalType: ModalState["type"]
  modalCallback?: () => void
}
export type ModalState = {
  visible: boolean
  type: "create-account" | "sign-in" | "safety-warning" | null
}

export const SlideDownModal = ({
  isVisibleModal,
  setModalState,
  modalType,
  modalCallback,
}: SlideDownModalProps) => {
  const [content, setContent] = React.useState<any>(null)
  const { width, height } = Dimensions.get("screen")
  const { navigate } = useNavigation()

  React.useEffect(() => {
    switch (modalType) {
      case "create-account": {
        setContent(modalContent["create-account"])
        break
      }
      case "sign-in": {
        setContent(modalContent["sign-in"])
        break
      }
      case "safety-warning": {
        setContent(modalContent["safety-warning"])
        break
      }
      default:
        setContent(null)
    }
  }, [modalType])

  const hideModal = () => {
    setModalState({ visible: false, type: null })
  }
  const onButtonPress = () => {
    switch (modalType) {
      case "create-account": {
        //@ts-ignore
        navigate("User Registration Screens")
        break
      }
      case "sign-in": {
        break
      }
      case "safety-warning": {
        modalCallback && modalCallback()
        break
      }
      default:
        return
    }
  }

  return (
    <Modal
      isVisible={isVisibleModal}
      onSwipeComplete={hideModal}
      onBackButtonPress={hideModal}
      onBackdropPress={hideModal}
      useNativeDriverForBackdrop
      useNativeDriver
      hideModalContentWhileAnimating
      swipeThreshold={100}
      swipeDirection={["down"]}
      deviceHeight={height}
      deviceWidth={width}
      style={{ margin: 0, flex: 1, alignItems: "flex-end" }}
      testID={"modal"}>
      {content ? (
        <>
          <View style={{ flex: 1, justifyContent: "flex-start" }} />
          <View style={[styles.modalView, { width }]}>
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
                <Text style={styles.header}>{content.header}</Text>
                <Text style={styles.subHeader}>{content.body}</Text>
              </View>
            </View>
            <View style={styles.buttonWrapper}>
              <FullWidthButton
                onPressCallback={onButtonPress}
                text={content.buttonTitle}
                colorScheme="light"
              />
            </View>
          </View>
        </>
      ) : (
        <></>
      )}
    </Modal>
  )
}

const styles = StyleSheet.create({
  modalView: {
    borderTopRightRadius: Sizing.x40,
    borderTopLeftRadius: Sizing.x40,
    flex: 3,
    justifyContent: "flex-end",
    alignItems: "center",
    backgroundColor: Colors.primary.neutral,
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
    height: "100%",
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
    marginBottom: "auto",
    alignItems: "center",
    width: "90%",
    height: "100%",
  },
})

const modalContent = {
  "create-account": {
    header: "Before We Start",
    body: "In the following screens, you will be prompted to provide some basic info about yourself (optional) and your username. Then we will show you your secret recovery phrase. This phrase gives you full control over your crypto assets and allows you to log in on a different device. Bonfire doesn't store any of your private keys on our servers.",
    buttonTitle: "I'm ready",
  },
  "sign-in": {
    header: "",
    body: "",
    buttonTitle: "I'm ready",
  },
  "safety-warning": {
    header: "Safety Warning",
    body: "On the next screen, we will show you your wallet recovery phrase. This is very sensitive information, so please make sure that no malicious actors are spying on you.",
    buttonTitle: "I'm safe",
  },
}
