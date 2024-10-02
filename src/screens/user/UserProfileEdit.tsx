import * as React from "react"
import { Pressable, StyleSheet, Text, View } from "react-native"

import { LeftArrowIcon } from "assets/icons"
import { Layout } from "components/layouts/basicLayout"
import { Colors, Typography, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"
import { UpdateAccountForm } from "components/forms/UpdateAccountForm"
import { ProfileStackParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"

interface UserProfileEditProps
  extends StackScreenProps<ProfileStackParamList, "Edit Profile"> {}

export const UserProfileEdit = ({ navigation, route }: UserProfileEditProps) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"

  const onBackNavigationPress = () => navigation.goBack()

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
        <UpdateAccountForm userInfo={route.params.userInfo} />
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
