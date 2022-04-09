import * as React from "react"
import { Pressable, StyleSheet, Text, TextInput, View } from "react-native"
import { LeftArrowIcon } from "assets/icons"
import { Layout } from "components/layouts/basicLayout"

import { Colors, Typography, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import {
  UserBaseDTO,
  UserOrganizerDTO,
} from "common/interfaces/profileInterface"
import { UpdateAccountForm } from "components/forms/UpdateAccountForm"
import { useUpdateAccountInfo } from "lib/hooks/useUpdateAccountInfo"

type AttendeeProfileInfo = UserBaseDTO
type OrganizerProfileInfo = AttendeeProfileInfo & UserOrganizerDTO

export const UserProfileEdit = ({ navigation }: any) => {
  const { colorScheme, accountType } = appContext()
  const { isUpdated } = useUpdateAccountInfo()
  const [newUserInfo, setNewUserInfo] = React.useState({
    name: "",
    username: "",
  })
  const [isInfoChanged, setIsInfoChanged] = React.useState<boolean>(false)
  const isLightMode = colorScheme === "light"

  const onBackNavigationPress = () => navigation.goBack()

  // const updateProfession = (value) => setNewUserInfo({...newUserInfo, prof

  return (
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
            Edit your account information
          </Text>
        </View>
        <UpdateAccountForm
          accountType={accountType || "attendee"}
          onInfoHasChanged={(val: boolean) => setIsInfoChanged(val)}
        />
      </View>
    </Layout>
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
