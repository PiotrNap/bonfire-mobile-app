import * as React from "react";
import { View, TextInput, Text, StyleSheet, Pressable } from "react-native";
import { Forms, Buttons, Colors, Sizing } from "styles/index";

export interface AddIdeaProps {
  submitHandler: (arg: string) => void;
}

export const AddIdea = ({ submitHandler }: AddIdeaProps) => {
  const [value, setValue] = React.useState<string>("");

  const onChangeText = (text: string) => {
    setValue(text);
  };

  return (
    <View style={styles.container}>
      <View style={styles.inputContainer}>
        <TextInput
          style={styles.inputText}
          placeholder="Add an Idea..."
          placeholderTextColor={Colors.neutral.s500}
          onChangeText={onChangeText}
        />
      </View>
      <Pressable
        style={Buttons.applyOpacity(styles.submitButton)}
        onPress={() => {
          submitHandler(value);
        }}>
        <Text style={styles.submitButtonText}>Submit</Text>
      </Pressable>
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    margin: Sizing.x20,
    alignItems: "center",
  },
  inputContainer: {
    width: "80%",
  },
  inputText: {
    ...Forms.input.primary,
  },
  submitButton: {
    marginTop: Sizing.x15,
    ...Buttons.bar.small,
  },
  submitButtonText: {
    ...Buttons.barText.small,
  },
});
