import * as React from "react";
import { View, StyleSheet, Pressable, Text } from "react-native";

import { Colors, Buttons, Sizing, Outlines, Typography } from "styles/index";

export interface NavigationButtonProps {
  onPress: () => void;
  Icon: React.ComponentClass<React.SVGProps<any>>;
}

export const NavigationButton = ({ onPress, Icon }: NavigationButtonProps) => {
  return (
    <View style={styles.navButtonWrapper}>
      <Pressable
        onPress={onPress}
        style={Buttons.applyOpacity(styles.navButton)}>
        <Icon stroke="#000" width={24} height={24} />
      </Pressable>
      <Text style={styles.navButtonSubTitle}>Wallet</Text>
    </View>
  );
};

const styles = StyleSheet.create({
  navButtonWrapper: {
    width: "20%",
    height: "100%",
    alignItems: "center",
    justifyContent: "center",
  },
  navButton: {
    width: "50%",
    height: "50%",
    alignItems: "center",
    justifyContent: "center",
    backgroundColor: Colors.neutral.s200,
    borderRadius: Sizing.x5,
    alignSelf: "center",
  },
  navButtonSubTitle: {
    ...Typography.body.x5,
  },
});
