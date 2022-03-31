import * as React from "react"
import { Pressable, StyleSheet, Text, View } from "react-native"
import { LeftArrowIcon } from "assets/icons"
import { Layout } from "screens/layouts/basicLayout"

import { Colors, Sizing } from "styles/index"
import { appContext } from "contexts/contextApi"

export const UserProfileEdit = ({ navigation }: any) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"

  const onBackNavigationPress = () => navigation.goBack()

  return (
    <Layout>
      <View style={styles.navigation}>
        <Pressable onPress={onBackNavigationPress} hitSlop={10}>
          <LeftArrowIcon
            width={24}
            height={24}
            color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
          />
        </Pressable>
      </View>
      <Text>Hello World</Text>
    </Layout>
  )
}

const styles = StyleSheet.create({
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
  },
})
