import * as React from "react"
import { View, Text, StyleSheet, Pressable, ScrollView } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { SafeAreaView, useSafeAreaInsets } from "react-native-safe-area-context"
import { appContext } from "contexts/contextApi"
import { LeftArrowIcon } from "assets/icons"
import FastImage from "react-native-fast-image"

interface EventBookingLayoutProps {
  children: React.ReactNode
  onBackPress: () => void
  screenHeader: string
  eventCardImage: string
  eventCardTitle: string
  eventCardColor: string
  eventCardTitleColor: string
}

export const EventBookingLayout = ({
  children,
  onBackPress,
  screenHeader,
  eventCardTitleColor,
  eventCardTitle,
  eventCardImage,
  eventCardColor,
}: EventBookingLayoutProps) => {
  const { colorScheme } = appContext()

  const isLightMode = colorScheme !== "dark"
  const insets = useSafeAreaInsets()

  return (
    <SafeAreaView style={{ flex: 1, paddingBottom: insets.bottom }}>
      <View style={styles.topContainer}>
        <FastImage
          source={{ uri: `data:image/png;base64,${eventCardImage}` }}
          style={styles.backgroundImage}>
          <View
            style={[
              styles.topInnerContainer,
              { backgroundColor: eventCardColor },
            ]}>
            <View style={[styles.topInnerWrapper, { paddingTop: insets.top }]}>
              <View style={styles.navigation}>
                <Pressable onPress={onBackPress} hitSlop={10}>
                  <LeftArrowIcon
                    width={24}
                    height={24}
                    color={Colors.primary.neutral}
                  />
                </Pressable>
              </View>
            </View>
            <View
              style={[
                styles.eventTitleWrapper,
                { paddingBottom: insets.bottom + Sizing.x15 },
              ]}>
              <Text
                ellipsizeMode="tail"
                numberOfLines={2}
                style={[styles.eventTitle, { color: eventCardTitleColor }]}>
                {eventCardTitle}
              </Text>
            </View>
          </View>
        </FastImage>
      </View>
      <ScrollView
        contentContainerStyle={[
          styles.bottomContainer,
          {
            backgroundColor: isLightMode
              ? Colors.primary.neutral
              : Colors.neutral.s600,
          },
        ]}>
        <View style={styles.timesHeader}>
          <Text
            style={
              isLightMode
                ? styles.timesHeaderText_light
                : styles.timesHeaderText_dark
            }>
            {screenHeader}
          </Text>
        </View>
        {children}
      </ScrollView>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  navigation: {
    flexDirection: "row",
    width: "90%",
    marginVertical: Sizing.x15,
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
    marginTop: Sizing.x25,
    marginBottom: Sizing.x10,
    marginRight: "auto",
    marginLeft: Sizing.x25,
  },
  timesHeaderText_light: {
    ...Typography.header.x50,
    color: Colors.primary.s800,
  },
  timesHeaderText_dark: {
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
  backgroundImage: {
    width: "100%",
    height: Sizing.x120,
    position: "absolute",
    top: 0,
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
    justifyContent: "space-between",
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
