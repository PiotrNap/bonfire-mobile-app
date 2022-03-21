import * as React from "react";
import {
  View,
  StyleSheet,
  Text,
  Pressable,
  ImageBackground,
} from "react-native";

import { Colors, Outlines, Sizing, Typography } from "styles/index";
import { appContext } from "contexts/contextApi";
import { HearthIcon } from "assets/icons/index";
import { ProfileTag } from "components/profile/profileTag";
import { BodyText } from "components/rnWrappers/bodyText";
import { scale } from "lib/utils";

// Remove this code once we have user data available
//@ts-ignore
import profilePicThree from "assets/images/profilePicThree.png";

export interface OrganizerProfileProps {
  profile: any;
}

export const OrganizerProfile = ({ profile }: OrganizerProfileProps) => {
  const { colorScheme, setFavoriteOrganizer, favoriteOrganizers } =
    appContext();

  const isFavorite =
    profile != null && favoriteOrganizers.includes(profile.alias);
  const isLightMode = colorScheme === "light";

  const onFavoritePress = () => {
    setFavoriteOrganizer(profile.alias);
  };

  const renderProfileTags = (tag: any, i: number) => {
    return (
      <View key={`${tag.tagName}_${i}_${profile.alias}`}>
        <ProfileTag tag={tag} alias={profile.alias} />
      </View>
    );
  };

  return (
    <View style={styles.container}>
      {profile && (
        <>
          <View style={styles.mainContainer}>
            <ImageBackground
              source={profilePicThree}
              imageStyle={styles.imageStyle}
              style={styles.imageBackground}
            />
            <View style={{ flex: 1 }}>
              <View style={styles.rightContainer}>
                <View style={styles.headerContainer}>
                  <Text
                    style={
                      isLightMode
                        ? styles.headerAlias_light
                        : styles.headerAlias_dark
                    }>
                    {profile.alias}
                  </Text>
                  <Text
                    style={
                      isLightMode
                        ? styles.headerOccupation_light
                        : styles.headerOccupation_dark
                    }>
                    {profile.occupation}
                  </Text>
                </View>
                <Pressable
                  onPress={onFavoritePress}
                  style={[
                    styles.favoriteButton,
                    !isFavorite
                      ? { ...Outlines.shadow.lifted }
                      : {
                          ...Outlines.shadow.base,
                        },
                  ]}>
                  <HearthIcon
                    width={24}
                    height={24}
                    color="#F87171"
                    strokeWidth={2}
                    fill={isFavorite ? "#F87171" : "transparent"}
                  />
                </Pressable>
              </View>
              <View style={styles.rightSubContainer}>
                <View style={styles.rightSubTop}>
                  <Text
                    style={
                      isLightMode
                        ? styles.relatedTags_light
                        : styles.relatedTags_dark
                    }>
                    Related tags
                  </Text>
                  <Text
                    style={
                      isLightMode
                        ? styles.hourlyRate_light
                        : styles.hourlyRate_dark
                    }>
                    50 ₳/hour
                  </Text>
                </View>
                {profile.tags != null && (
                  <View style={styles.rightSubBottom}>
                    {profile.tags.map(renderProfileTags)}
                  </View>
                )}
              </View>
            </View>
          </View>
          <View style={styles.descriptionContainer}>
            <BodyText colors={[Colors.primary.s600, Colors.primary.neutral]}>
              Lorem ipsum dolor sit amet, consectetur adipiscing elit.
              Vestibulum venenatis quam sem, eget bibendum lorem convallis et.
              Donec velit ante, efficitur at ante eu, consequat hendrerit augue.
              Vivamus quis eros ex
            </BodyText>
          </View>
        </>
      )}
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    flexBasis: "auto",
    width: "90%",
    alignItems: "flex-end",
  },
  mainContainer: {
    alignItems: "center",
    width: "100%",
    flexDirection: "row",
    paddingLeft: Sizing.x10,
  },
  imageStyle: {
    width: "100%",
    height: "100%",
    resizeMode: "cover",
    borderRadius: 999,
  },
  imageBackground: {
    width: scale(98),
    height: scale(98),
    borderRadius: 999,
    ...Outlines.shadow.lifted,
  },
  rightContainer: {
    marginLeft: Sizing.x15,
    marginBottom: Sizing.x8,
    flexDirection: "row",
  },
  headerContainer: {},
  headerAlias_light: {
    ...Typography.header.x40,
    color: Colors.primary.s800,
  },
  headerAlias_dark: {
    ...Typography.header.x40,
    color: Colors.primary.neutral,
  },
  headerOccupation_light: {
    ...Typography.subHeader.x25,
    color: Colors.primary.s600,
  },
  headerOccupation_dark: {
    ...Typography.subHeader.x25,
    color: Colors.primary.s600,
  },
  favoriteButton: {
    backgroundColor: "#FEE2E2",
    width: Sizing.x35,
    height: Sizing.x35,
    alignItems: "center",
    justifyContent: "center",
    marginLeft: "auto",
    marginRight: Sizing.x5,
    borderRadius: Outlines.borderRadius.base,
  },
  rightSubContainer: {},
  rightSubTop: {
    flexDirection: "row",
    justifyContent: "space-between",
    alignItems: "center",
    marginLeft: Sizing.x15,
    marginBottom: Sizing.x2,
  },
  relatedTags_light: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s800,
  },
  relatedTags_dark: {
    ...Typography.subHeader.x10,
    color: Colors.primary.s200,
  },
  hourlyRate_light: {
    ...Typography.subHeader.x25,
    color: Colors.primary.s800,
  },
  hourlyRate_dark: {
    ...Typography.subHeader.x25,
    color: Colors.primary.s200,
  },
  rightSubBottom: {
    flexDirection: "row",
    marginLeft: Sizing.x15,
  },
  descriptionContainer: {
    marginVertical: Sizing.x10,
    marginHorizontal: Sizing.x5,
  },
});
