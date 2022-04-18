import { HearthIcon } from "assets/icons"
import { BodyText } from "components/rnWrappers/bodyText"
import { appContext } from "contexts/contextApi"
import * as React from "react"
import { View, Text } from "react-native"
import { Colors, Sizing, Typography } from "styles/index"

export interface Props {
  views: number
  bookings: number
  likes?: number
}

export const EventStatistics = ({ views, bookings, likes }: Props) => {
  const { colorScheme } = appContext()
  const isLightMode = colorScheme === "light"
  const TextComponent = React.useCallback(
    ({ children }: any) => (
      <BodyText
        customStyle={{ fontFamily: "Roboto-Medium" }}
        changingColorScheme
        colors={[Colors.primary.s800, Colors.primary.neutral]}>
        {children}
      </BodyText>
    ),
    [colorScheme]
  )

  return (
    <View style={{ flexDirection: "row" }}>
      <View style={{ width: "90%" }}>
        <TextComponent>{bookings} bookings</TextComponent>
        <TextComponent>{views} views</TextComponent>
      </View>
      <View
        style={{
          marginRight: "auto",
          alignItems: "center",
          justifyContent: "center",
        }}>
        {likes != null ? (
          <>
            <HearthIcon
              style={{ width: Sizing.x25, height: Sizing.x25 }}
              stroke={
                isLightMode ? Colors.primary.s600 : Colors.primary.neutral
              }
            />
            <TextComponent>
              <Text style={{ ...Typography.fontWeight.semibold }}>{likes}</Text>
            </TextComponent>
          </>
        ) : (
          <></>
        )}
      </View>
    </View>
  )
}
