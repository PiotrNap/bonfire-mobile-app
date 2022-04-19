import * as React from "react"
import { Pressable, StyleSheet, Text, View } from "react-native"

import { CheckIcon, LeftArrowIcon } from "assets/icons"
import { Layout } from "components/layouts/basicLayout"
import { SlideTopModal } from "components/modals/slideTopModal"
import { ErrorIcon } from "assets/icons"
import { Colors, Typography, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import { UpdateAccountForm } from "components/forms/UpdateAccountForm"
import { ProfileStackParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"

interface UserProfileEditProps extends StackScreenProps<ProfileStackParamList, "Edit Profile"> {}

export const UserProfileEdit = ({ navigation, route }: UserProfileEditProps) => {
  const { colorScheme, accountType } = appContext()
  const [isInfoChanged, setIsInfoChanged] = React.useState<boolean>(false)
  const [updateResponse, setUpdateResponse] = React.useState<any>(null)
  const [isModalVisible, setIsModalVisible] = React.useState<boolean>(false)
  const isLightMode = colorScheme === "light"

  const onBackNavigationPress = () => navigation.goBack()
  const ModalIcon = React.memo(() =>
    isErrorResponse ? (
      <ErrorIcon
        stroke={Colors.primary.neutral}
        width={Sizing.x60}
        height={Sizing.x60}
        strokeWidth={1.5}
      />
    ) : (
      <CheckIcon
        stroke={Colors.success.s400}
        width={Sizing.x60}
        height={Sizing.x60}
        strokeWidth={1.5}
      />
    )
  )
  const updateResponseMsg = (val: any) => {
    setUpdateResponse(val)
    setIsModalVisible(true)
  }
  const modalHideCallback = () => {
    setUpdateResponse(null)
    setIsModalVisible(false)
  }
  const isErrorResponse = updateResponse?.status !== 201

  return (
    <>
      <Layout scrollable>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <View style={styles.formContainer}>
          <View style={styles.pageHeader}>
            <Text
              style={
                isLightMode
                  ? styles.pageHeaderText_light
                  : styles.pageHeaderText_dark
              }>
              Add or change info
            </Text>
          </View>
          <UpdateAccountForm
            userInfo={route.params.userInfo}
            accountType={accountType || "attendee"}
            onInfoHasChanged={(val: boolean) => setIsInfoChanged(val)}
            onUpdateResponse={updateResponseMsg}
          />
        </View>
      </Layout>
      {updateResponse && (
        <SlideTopModal
          icon={<ModalIcon />}
          isModalVisible={isModalVisible}
          modalContent={updateResponse?.msg}
          backgroundColor={
            isErrorResponse ? Colors.danger.s300 : Colors.primary.s180
          }
          contentStyle={!isErrorResponse ? { color: Colors.success.s400 } : {}}
          hideCallback={modalHideCallback}
        />
      )}
    </>
  )
}

const styles = StyleSheet.create({
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginTop: Sizing.x15,
  },
  formContainer: {
    flex: 1,
    marginVertical: Sizing.x10,
    width: "90%",
  },
  pageHeader: {
    marginVertical: Sizing.x5,
    marginRight: "auto",
  },
  pageHeaderText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  pageHeaderText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
})
