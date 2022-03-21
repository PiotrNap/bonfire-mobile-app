import * as React from "react";
import { View, StyleSheet, Text, TextInput } from "react-native";

import { CustomPlainInput } from "components/forms/CustomPlainInput";
import { Typography, Colors, Sizing, Forms } from "styles/index";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { ProfileContext } from "contexts/profileContext";
import { KeyboardAwareScrollView } from "react-native-keyboard-aware-scroll-view";

export interface UserDetailScreenProps {}

export const UserDetailsScreen = ({ pagerRef }: any) => {
  const { setProfession, setJobTitle, setBio, setSkills, setTimeBlockCostADA } =
    React.useContext(ProfileContext);
  const [_profession, _setProfession] = React.useState<string>("");
  const [_jobTitle, _setJobTitle] = React.useState<string>("");
  const [_bio, _setBio] = React.useState<string>("");
  const [_timeBlockCostAda, _setTimeBlockCostAda] = React.useState<number>(0);
  const [_skills, _setSkills] = React.useState<string>("");

  const submitBioState = () => {
    setProfession(_profession);
    setJobTitle(_jobTitle);
    setBio(_bio);
    setTimeBlockCostADA(_timeBlockCostAda);
    setSkills(_skills);
    pagerRef.current.setPage(1);
  };

  return (
    <KeyboardAwareScrollView
      keyboardShouldPersistTaps="handled"
      showsVerticalScrollIndicator={false}
      keyboardOpeningTime={Number.MAX_SAFE_INTEGER}
      style={{ width: "90%", marginVertical: Sizing.x15 }}>
      <View style={styles.header}>
        <Text style={styles.headerText}>
          Tell us a little bit{"\n"}about you
        </Text>
      </View>
      <View style={styles.formContainer}>
        <CustomPlainInput
          label="Profession"
          placeholder="Doctor, therapist, developer..."
          styles={inputStyles}
          isLightMode={false}
          onChangeCallback={(val) => _setProfession(val)}
        />
        <CustomPlainInput
          label="Job Title"
          placeholder="Full Stack Engineer, Sr Business..."
          styles={inputStyles}
          isLightMode={false}
          onChangeCallback={(val) => _setJobTitle(val)}
        />
        {/* when handling events with multiline, use ref._lastNativeText */}
        <CustomPlainInput
          label="About yourself"
          multiline={true}
          numberOfLines={8}
          maxChar={250}
          placeholder="Passionate in helping others draw business goals and needs..."
          styles={inputStyles}
          isLightMode={false}
          onChangeCallback={(val) => _setBio(val)}
        />
        {/*<CustomPlainInput
          label="Availability"
          placeholder="weekly, weekends, mornings, e..."
          styles={styles}
          isLightMode={false}
          onChangeCallback={() => {}}
        />*/}
        <CustomPlainInput
          label="Skills"
          placeholder="Organized, Motivated, Critical Th..."
          styles={inputStyles}
          isLightMode={false}
          onChangeCallback={(val) => _setSkills(val)}
        />
      </View>
      <View style={inputStyles.inputContainer}>
        <View style={inputStyles.labelContainer}>
          <Text style={inputStyles.label}>Hourly rate</Text>
        </View>
        <View style={inputStyles.textInputWrapper}>
          <TextInput
            style={inputStyles.input}
            keyboardType="numeric"
            textContentType="none"
            placeholder="35 ₳ an hour"
            placeholderTextColor={inputStyles.placeholderText.color}
            onChangeText={(val) => _setTimeBlockCostAda(Number(val))}
          />
        </View>
      </View>
      <FullWidthButton
        onPressCallback={submitBioState}
        text="Next"
        buttonType="transparent"
        colorScheme="dark"
      />
    </KeyboardAwareScrollView>
  );
};

const styles = StyleSheet.create({
  container: {
    width: "90%",
  },
  scrollView: {
    width: "100%",
    height: "100%",
    marginVertical: Sizing.x20,
  },
  header: {
    marginVertical: Sizing.x10,
  },
  headerText: {
    ...Typography.header.x65,
    color: Colors.primary.neutral,
  },
  formContainer: {
    flex: 1,
    marginVertical: Sizing.x10,
  },
});
/**
 * styles passed as prop to CustomPlainInput
 */
const inputStyles = StyleSheet.create({
  inputContainer: {
    flex: 1,
    width: "100%",
    alignItems: "center",
    marginBottom: Sizing.x10,
  },
  labelContainer: {
    width: "100%",
  },
  label: {
    ...Forms.inputLabel.primary,
  },
  textInputWrapper: {
    width: "100%",
    flexDirection: "row",
    alignItems: "center",
  },
  input: {
    width: "100%",
    ...Forms.input.primary,
  },
  placeholderText: {
    color: Colors.primary.s300,
  },
});
