import * as React from "react";
import {
  ImageBackground,
  Pressable,
  StyleSheet,
  Text,
  View,
} from "react-native";

import { appContext } from "contexts/contextApi";
import { Colors } from "styles/index";
import { ProfileContext } from "contexts/profileContext";

export const ImageSelectionSection = () => {
  const { colorScheme } = appContext();
  // const { currImage } = ProfileContext();
  const currImage = "";

  return (
    // <ImageBackground
    //   // only for testing purpose render uri conditionally
    //   source={{ uri: currImage }}
    //   imageStyle={styles.profilePicImage}
    //   style={[
    //     styles.profilePic,
    //     darkMode
    //       ? {
    //           borderColor: Colors.primary.neutral,
    //           borderWidth: Outlines.borderWidth.base,
    //         }
    //       : {},
    //   ]}>
    //   <Pressable
    //     onPressIn={() => {}}
    //     onPressOut={() => {}}
    //     onLongPress={() => {}}
    //     hitSlop={5}
    //     pressRetentionOffset={5}
    //     style={styles.profilePicWrapper}>
    //     <View style={styles.profilePicEdit}>
    //       <Text style={styles.profilePicEditText}>Edit</Text>
    //     </View>
    //   </Pressable>
    // </ImageBackground>
    <></>
  );
};

const styles = StyleSheet.create({});
