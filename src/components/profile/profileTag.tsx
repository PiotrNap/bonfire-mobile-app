import * as React from "react";
import { Text, View, StyleSheet } from "react-native";

import { Outlines, Sizing, Tags } from "styles/index";

export interface ProfileTagProps {
  tag: any;
  alias?: string;
}

export const ProfileTag = ({ tag }: ProfileTagProps) => {
  return (
    <View
      style={[styles.skillTag, { backgroundColor: tag.tagBackgroundColor }]}>
      <Text style={[styles.skillName, { color: tag.tagTextColor }]}>
        {tag.tagName}
      </Text>
    </View>
  );
};

const styles = StyleSheet.create({
  skillTag: {
    borderRadius: Outlines.borderRadius.large,
    paddingHorizontal: Sizing.x12,
    marginRight: Sizing.x8,
    textAlign: "center",
  },
  skillName: {
    ...Tags.tagHeader.small,
  },
});
