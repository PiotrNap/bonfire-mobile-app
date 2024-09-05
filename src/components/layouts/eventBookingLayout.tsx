import * as React from "react"
import { View, Text, StyleSheet, Pressable, ScrollView } from "react-native"

import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { appContext } from "contexts/contextApi"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { Layout } from "./basicLayout"

interface EventBookingLayoutProps {
  children: React.ReactNode
  onBackPress: () => void
  screenHeader: string
  screenSubHeader?: string
  eventCardImage: string
  eventCardTitle: string
  eventCardColor: string
  eventCardTitleColor: string
}

export const EventBookingLayout = ({
  children,
  screenHeader,
  screenSubHeader,
}: EventBookingLayoutProps) => {
  const { colorScheme } = appContext()

  const isLightMode = colorScheme !== "dark"
  return (
    <Layout backNavigationIcon scrollable>
      <View style={styles.timesHeader}>
        <Text style={isLightMode ? styles.headerText_light : styles.headerText_dark}>
          {screenHeader}
        </Text>
        {screenSubHeader && (
          <SubHeaderText
            customStyle={styles.subHeader}
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            {screenSubHeader}
          </SubHeaderText>
        )}
      </View>
      {children}
    </Layout>
  )
}

const styles = StyleSheet.create({
  navigation: {
    borderRadius: Outlines.borderRadius.max,
    backgroundColor: Colors.primary.neutral,
    marginBottom: "auto",
    width: Sizing.x40,
    height: Sizing.x40,
    marginTop: Sizing.x15,
    alignItems: "center",
    justifyContent: "center",
  },
  topContainer: {
    height: Sizing.x100,
  },
  bottomContainer: {
    flexGrow: 1,
    alignItems: "center",
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  bottomWrapper: {
    flex: 1,
    width: "90%",
    paddingVertical: Sizing.x20,
    justifyContent: "space-between",
  },
  timesHeader: {
    marginTop: Sizing.x10,
    marginBottom: Sizing.x10,
    marginRight: "auto",
    marginLeft: Sizing.x25,
  },
  headerText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  headerText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
  subHeader: {
    ...Typography.subHeader.x10,
    paddingLeft: Sizing.x5,
    paddingBottom: Sizing.x5,
  },
  backgroundImage: {
    width: "100%",
    height: Sizing.x120,
    position: "absolute",
    top: 0,
    bottom: 0,
    lef: 0,
    right: 0,
  },
  topInnerContainer: {
    height: "100%",
    alignItems: "center",
    justifyContent: "flex-start",
    paddingBottom: Sizing.x15,
  },
  topInnerWrapper: {
    width: "90%",
    flexDirection: "row",
  },
  eventTitleWrapper: {
    width: "90%",
    marginTop: "auto",
    marginBottom: Sizing.x20,
  },
  eventTitle: {
    ...Typography.header.x55,
    color: Colors.primary.neutral,
  },
})
