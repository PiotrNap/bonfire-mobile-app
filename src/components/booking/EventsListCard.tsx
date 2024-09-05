import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { useNavigation } from "@react-navigation/native"
import tinyColor from "tinycolor2"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { applyOpacity } from "../../styles/colors"
import { getEventCardDate } from "lib/utils"
import FastImage from "react-native-fast-image"
import LinearGradient from "react-native-linear-gradient"

export interface EventsListCardProps {
  title: string
  description: string
  eventId?: string
  organizerId?: string
  organizerAlias?: string
  fromDate: Date
  toDate: Date
  image: any
  color: string
  titleColor: string
  hourlyRate?: any
  defaultCardColor?: boolean
  isEventCardPreview?: boolean
  isBrowseScreenPreview?: boolean
  isTransparent?: boolean
  isStandardColor?: boolean
  bookedSlots?: any[]
}

export const EventsListCard = ({
  isEventCardPreview,
  isTransparent,
  isStandardColor,
  eventId,
  organizerId,
  organizerAlias,
  title,
  description,
  fromDate,
  toDate,
  image,
  color,
  titleColor,
  hourlyRate,
  bookedSlots,
}: EventsListCardProps) => {
  const navigation = useNavigation()
  const _color = tinyColor(color)

  const onCardPress = () =>
    navigation.navigate("Event Description", {
      title,
      description,
      fromDate,
      toDate,
      image,
      eventId,
      organizerId,
      organizerAlias,
      color,
      titleColor,
      hourlyRate,
      isStandardColor,
      bookedSlots,
    })
  const gradient: string[] =
    isEventCardPreview && isTransparent
      ? [Colors.primary.s800, Colors.primary.s600]
      : (isEventCardPreview && !isTransparent) || !isStandardColor
      ? [color, color]
      : [Colors.primary.s800, Colors.primary.s600]

  console.log(gradient)

  const Background = React.useCallback(
    ({ children }) => (
      <View
        style={{
          flex: 1,
          justifyContent: "center",
          alignItems: "center",
        }}>
        {image && (
          <FastImage
            resizeMode={FastImage.resizeMode.cover}
            source={{
              uri: isEventCardPreview ? image : `data:image/png;base64,${image}`,
            }}
            style={[styles.background, { zIndex: 0 }]}
          />
        )}
        {!isTransparent && (
          <LinearGradient
            colors={gradient}
            start={{ x: 0, y: 1 }}
            end={{ x: 1, y: 0 }}
            style={[styles.background, { zIndex: 10 }]}
          />
        )}
        <View style={{ zIndex: 20, width: "90%", height: "90%" }}>{children}</View>
      </View>
    ),
    [image, color]
  )

  return (
    <Pressable
      disabled={isEventCardPreview ? true : false}
      onPress={onCardPress}
      style={styles.main}>
      <Background>
        {fromDate != null && toDate != null && (
          <View style={styles.dateCard}>
            <Text style={styles.dateCardText}>{getEventCardDate(fromDate, toDate)}</Text>
          </View>
        )}

        <Text
          style={[
            styles.eventTitle,
            titleColor && !isTransparent ? { color: titleColor } : {},
          ]}
          ellipsizeMode="tail"
          numberOfLines={2}>
          {title}
        </Text>
      </Background>
    </Pressable>
  )
}

const styles = StyleSheet.create({
  main: {
    height: Sizing.x120,
    marginVertical: Sizing.x10,
    ...Outlines.shadow.base,
    borderRadius: Outlines.borderRadius.base,
  },
  background: {
    flex: 1,
    width: "100%",
    height: Sizing.x120,
    padding: Sizing.x15,
    position: "absolute",
    left: 0,
    right: 0,
    top: 0,
    bottom: 0,
    borderRadius: Outlines.borderRadius.base,
  },
  dateCard: {
    maxWidth: Sizing.x80,
    height: "auto",
    marginLeft: "auto",
    borderRadius: Outlines.borderRadius.small,
    backgroundColor: applyOpacity("000000", 0.3),
  },
  dateCardText: {
    textAlign: "center",
    padding: Sizing.x5,
    ...Typography.header.x30,
    color: Colors.primary.neutral,
  },
  eventTitle: {
    // maxWidth: "85%",
    marginTop: "auto",
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
})
