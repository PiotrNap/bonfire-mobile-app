import * as React from "react";
import { Pressable, StyleSheet, View } from "react-native";

import { LeftArrowIcon } from "assets/icons";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { HeaderText } from "components/rnWrappers/headerText";
import { appContext } from "contexts/contextApi";
import { SafeAreaView } from "react-native-safe-area-context";
import { Colors, Sizing } from "styles/index";

export interface AvailabilitiesCreationProps {
  navigation: any;
  route: any;
}

export const AvailabilitiesCreation = ({
  navigation,
  route,
}: AvailabilitiesCreationProps) => {
  const { colorScheme } = appContext();

  const isLightMode = colorScheme === "light";
  const isDisabledButton = true;

  // navigation handlers
  const onBackNavigationPress = () => navigation.goBack();
  const onNextPress = () => navigation.navigate("Select Availabilities");

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode
            ? Colors.primary.neutral
            : Colors.primary.s600,
        },
      ]}>
      <View style={{ flex: 1, width: "100%", alignItems: "center" }}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <HeaderText
          customStyles={{ marginBottom: Sizing.x10 }}
          colorScheme={colorScheme}>
          Select a time you are available
        </HeaderText>
        <FullWidthButton
          text="Next"
          colorScheme={colorScheme}
          disabled={isDisabledButton}
          onPressCallback={onNextPress}
          style={styles.button}
        />
      </View>
    </SafeAreaView>
  );
};

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    marginVertical: Sizing.x15,
    alignSelf: "flex-start",
    width: "90%",
  },
  button: { width: "90%", marginTop: "auto", marginBottom: Sizing.x15 },
});
