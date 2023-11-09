import * as React from "react"
import { View, Text, StyleSheet, Dimensions, Pressable } from "react-native"

import PagerView from "react-native-pager-view"
import { CreateAccountForm } from "components/forms/CreateAccountForm"
import { Buttons, Colors, Sizing, Typography } from "styles/index"
import { BusinessDecisionsIcon, LeftArrowIcon, ModernProfessionalIcon } from "icons/index"
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view"
import { ProfileContext } from "contexts/profileContext"
import { SlideTopModal } from "components/modals/SlideTopModal"
import { ErrorIcon } from "assets/icons"
import { Errors } from "common/types/errors"
import { HeaderText } from "components/rnWrappers/headerText"

const SCREEN_WIDTH = Dimensions.get("screen").width

export interface CreateAccountScreenProps {
  pagerRef: React.RefObject<PagerView>
}

export const CreateAccountScreen = ({ pagerRef }: CreateAccountScreenProps) => {
  const { setUsername } = React.useContext(ProfileContext)
  const [modalVisible, setModalVisible] = React.useState<boolean>(false)
  const [errorType, setErrorType] = React.useState<string>("")

  const onBackPress = () => {
    setProfileType("")
    setUsername("")
    pagerRef.current?.setPage(1)
  }

  const onErrorCallback = (errorType: string) => {
    setErrorType(errorType)
    setModalVisible(true)
  }

  const onChangeCallback = () => {
    setModalVisible(false)
  }

  const errorHideCallback = () => {
    setModalVisible(false)
  }

  const ErrorModalIcon = React.memo(() => (
    <ErrorIcon
      stroke={Colors.primary.neutral}
      width={Sizing.x60}
      height={Sizing.x60}
      strokeWidth={1.5}
    />
  ))

  return (
    <>
      <KeyboardAwareScrollView
        keyboardShouldPersistTaps="handled"
        showsVerticalScrollIndicator={false}
        keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
        style={{ width: "90%" }}
        contentContainerStyle={{ alignItems: "center" }}>
        <View style={styles.imageContainer}>
          {profileType === "attendee" ? (
            <ModernProfessionalIcon style={styles.image} width="80%" height="80%" />
          ) : profileType === "organizer" ? (
            <BusinessDecisionsIcon style={styles.image} width="80%" height="80%" />
          ) : (
            <></>
          )}
        </View>
        <View style={styles.header}>
          <HeaderText>Create New Account</HeaderText>
        </View>
        <CreateAccountForm
          onChangeCallback={onChangeCallback}
          onErrorCallback={onErrorCallback}
        />
        <View style={styles.backButtonSection}>
          <Pressable
            onPress={onBackPress}
            style={Buttons.applyOpacity(styles.backButton)}>
            <Text style={styles.backButtonText}>Back</Text>
            <LeftArrowIcon
              color={Colors.primary.neutral}
              width={18}
              height={18}
              strokeWidth={4}
              style={styles.backButtonIcon}
            />
          </Pressable>
        </View>
      </KeyboardAwareScrollView>
    </>
  )
}

const styles = StyleSheet.create({
  container: {
    width: "90%",
  },
  scrollView: {
    alignItems: "center",
  },
  imageContainer: {
    width: SCREEN_WIDTH * 0.5,
    height: SCREEN_WIDTH * 0.5,
    marginVertical: -Sizing.x10,
    alignItems: "center",
    justifyContent: "center",
  },
  image: {
    alignSelf: "center",
  },
  header: {
    width: "100%",
    marginBottom: Sizing.x15,
  },
  headerText: {
    width: "100%",
    ...Typography.header.x60,
    color: Colors.primary.neutral,
  },
  backButtonSection: {
    marginTop: Sizing.x20,
  },
  backButton: {
    justifyContent: "center",
  },
  backButtonText: {
    paddingBottom: Sizing.x2,
    marginLeft: Sizing.x8,
    ...Typography.subHeader.x35,
    ...Typography.roboto.medium,
    color: Colors.primary.neutral,
  },
  backButtonIcon: {
    position: "absolute",
    left: -Sizing.x12,
  },
})
