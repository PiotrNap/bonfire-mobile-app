import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { useNavigation } from "@react-navigation/native"
import tinyColor from "tinycolor2"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { applyOpacity } from "../../styles/colors"
import { getEventCardDate } from "lib/utils"
import FastImage from "react-native-fast-image"
import { appContext } from "contexts/contextApi"
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
}

export const EventsListCard = ({
  isEventCardPreview,
  isTransparent,
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
    })
  const gradient: string[] = [Colors.primary.s800, Colors.primary.s600]

  const Background = React.useCallback(
    ({ children }) =>
      image ? (
        <FastImage
          source={{
            uri: isEventCardPreview ? image : `data:image/png;base64,${image}`,
          }}
          style={styles.background}>
          {children}
        </FastImage>
      ) : (
        <LinearGradient
          colors={gradient}
          start={{ x: 0, y: 1 }}
          end={{ x: 1, y: 0 }}
          style={styles.background}>
          {children}
        </LinearGradient>
      ),
    [image]
  )

  return (
    <Pressable
      disabled={isEventCardPreview ? true : false}
      onPress={onCardPress}
      style={styles.main}>
      <Background>
        {fromDate != null && toDate != null && (
          <View
            style={[
              styles.dateCard,
              isTransparent
                ? {
                    backgroundColor: applyOpacity("000000", 0.3),
                  }
                : {
                    backgroundColor: applyOpacity(_color.toHexString(), 0.5),
                  },
            ]}>
            <Text style={styles.dateCardText}>{getEventCardDate(fromDate, toDate)}</Text>
          </View>
        )}

        <Text
          style={[
            styles.eventTitle,
            titleColor && !isTransparent ? { color: titleColor } : {},
          ]}>
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
    borderRadius: Outlines.borderRadius.base,
    backgroundColor: Colors.primary.s600,
  },
  dateCard: {
    maxWidth: Sizing.x80,
    height: "auto",
    marginLeft: "auto",
    borderRadius: Outlines.borderRadius.small,
  },
  dateCardText: {
    textAlign: "center",
    padding: Sizing.x5,
    ...Typography.header.x30,
    color: Colors.primary.neutral,
  },
  eventTitle: {
    maxWidth: "85%",
    marginTop: "auto",
    ...Typography.header.x50,
    color: Colors.primary.neutral,
  },
})
