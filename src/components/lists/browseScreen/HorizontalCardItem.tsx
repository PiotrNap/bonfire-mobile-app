import * as React from "react";
import { ImageBackground, StyleSheet, Text, View } from "react-native";
import { Colors, Outlines, Sizing, Typography } from "styles/index";

// Remove this code once we have external images.
/* @ts-ignore */
import investing from "assets/images/investing.jpg";
/* @ts-ignore */
import developing from "assets/images/developing.jpg";
/* @ts-ignore */
import clothes from "assets/images/clothes.jpg";
/* @ts-ignore */
import health from "assets/images/health.png";
/* @ts-ignore */
import lawyer from "assets/images/lawyer.png";
/* @ts-ignore */
import fashion from "assets/images/fashion.png";

const Images = [
  { image: investing, title: "Investing" },
  { image: developing, title: "Developing" },
  { image: clothes, title: "Fashion" },
  { image: health, title: "Health" },
  { image: lawyer, title: "Lawyer" },
  { image: fashion, title: "Fashion" },
];

export interface HorizontalCardItemProps {
  item: any;
  navigateTo: any;
}

export const HorizontalCardItem = ({ item }: HorizontalCardItemProps) => {
  const placeholder: any = Images[Math.floor(Math.random() * Images.length)];

  return (
    <ImageBackground
      imageStyle={styles.backgroundImage}
      source={placeholder.image}
      style={styles.container}>
      <View
        style={[
          styles.innerContainer,
          { backgroundColor: Colors.applyOpacity(item.backgroundColor, 0.5) },
        ]}>
        <Text style={styles.innerText}>{placeholder.title}</Text>
      </View>
    </ImageBackground>
  );
};

const styles = StyleSheet.create({
  container: {
    height: Sizing.x110,
    width: Sizing.x110,
    alignItems: "center",
    justifyContent: "center",
    marginRight: Sizing.x12,
    ...Outlines.shadow.lifted,
    elevation: 4,
  },
  backgroundImage: {
    borderRadius: Outlines.borderRadius.base,
  },
  innerContainer: {
    flex: 1,
    width: "100%",
    justifyContent: "center",
    alignItems: "center",
    borderRadius: Outlines.borderRadius.base,
  },
  innerText: {
    ...Typography.header.x30,
    color: Colors.primary.neutral,
  },
});
