import * as React from "react"
import { View, Text, StyleSheet, Pressable, ScrollView } from "react-native"

import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { SafeAreaView, useSafeAreaInsets } from "react-native-safe-area-context"
import { appContext } from "contexts/contextApi"
import { LeftArrowIcon } from "assets/icons"
import FastImage from "react-native-fast-image"
import LinearGradient from "react-native-linear-gradient"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"

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
  onBackPress,
  screenHeader,
  screenSubHeader,
  eventCardTitleColor,
  eventCardTitle,
  eventCardImage,
  eventCardColor,
}: EventBookingLayoutProps) => {
  const { colorScheme } = appContext()
  const isStandardColor =
    (eventCardTitleColor === "white" && eventCardColor === "transparent") ||
    (eventCardTitleColor === "rgb(255, 252, 252)" &&
      eventCardColor === "rgba(3, 3, 3, 0)")
  const gradient: string[] = !isStandardColor
    ? [eventCardColor, eventCardColor]
    : [Colors.primary.s800, Colors.primary.s600]

  const Background = React.useCallback(
    ({ children }) =>
      eventCardImage ? (
        <FastImage
          source={{ uri: `data:image/png;base64,${eventCardImage}` }}
          style={styles.backgroundImage}>
          {children}
        </FastImage>
      ) : (
        <LinearGradient
          colors={gradient}
          start={{ x: 0, y: 1 }}
          end={{ x: 1, y: 0 }}
          style={styles.backgroundImage}>
          {children}
        </LinearGradient>
      ),
    [eventCardImage]
  )

  const isLightMode = colorScheme !== "dark"
  const insets = useSafeAreaInsets()
  return (
    <SafeAreaView style={{ flex: 1, paddingBottom: insets.bottom }}>
      <View style={styles.topContainer}>
        <Background>
          <View style={styles.topInnerContainer}>
            <View style={[styles.topInnerWrapper, { paddingTop: insets.top }]}>
              <Pressable
                style={Buttons.applyOpacity(styles.navigation)}
                onPress={onBackPress}
                hitSlop={10}>
                <LeftArrowIcon width={24} height={24} color={Colors.primary.s600} />
              </Pressable>
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
        </Background>
      </View>
      <ScrollView
        contentContainerStyle={[
          styles.bottomContainer,
          {
            backgroundColor: isLightMode ? Colors.primary.neutral : Colors.neutral.s600,
          },
        ]}>
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
      </ScrollView>
    </SafeAreaView>
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
    marginTop: Sizing.x25,
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
