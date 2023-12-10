import * as React from "react"
import { View, StyleSheet, Animated } from "react-native"

import { Layout } from "components/layouts/basicLayout"
import { EventsTabs } from "components/tabs/eventsTabs"

export const MyEvents = ({ navigation, route }: any) => {
  const params = route.params
  /* @TODO Maybe once users have enough events going on we can add a search bar...*/
  return (
    <Layout>
      <View style={styles.main}>
        <EventsTabs reload={params?.reload} />
      </View>
    </Layout>
  )
}

const styles = StyleSheet.create({
  main: {
    flex: 1,
    width: "100%",
  },
  topContainer: {
    width: "90%",
  },
})
