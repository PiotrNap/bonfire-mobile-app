import * as React from "react";
import { View, Text, StyleSheet, Pressable } from "react-native";
import { SafeAreaView } from "react-native-safe-area-context";
import { Buttons, Typography, Sizing, Outlines } from "styles/index";

export const AttendeesScreen = ({ navigation }: any) => {
  return (
    <SafeAreaView style={styles.safeArea}>
      <View style={styles.header}>
        <Text style={styles.headerText}>Attendees</Text>
      </View>
      <View style={styles.body}>
        <Pressable
          style={Buttons.applyOpacity(styles.button)}
          onPress={() => {}}>
          <Text style={styles.buttonText}>How it works</Text>
        </Pressable>
        <Pressable
          style={Buttons.applyOpacity(styles.button)}
          onPress={() => navigation.navigate("Attendees")}>
          <Text style={styles.buttonText}>Create Account</Text>
        </Pressable>
        <Pressable
          style={Buttons.applyOpacity(styles.button)}
          onPress={() => navigation.navigate("Browse")}>
          <Text style={styles.buttonText}>Browse</Text>
        </Pressable>
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
  },
  top: {
    margin: Sizing.x10,
  },
  header: {
    marginTop: Sizing.x20,
    marginBottom: Sizing.x60,
    padding: Sizing.x20,
  },
  headerText: {
    ...Typography.header.x60,
    marginHorizontal: Sizing.x5,
    marginTop: Sizing.x40,
    alignSelf: "center",
  },
  body: {
    alignItems: "center",
  },
  button: {
    ...Outlines.shadow.base,
    width: Sizing.x130,
    margin: Sizing.x10,
  },
  buttonText: {},
});
