import * as React from "react";
import { View, StyleSheet, Pressable, TextInput } from "react-native";
import { Buttons, Typography, Sizing, Outlines } from "styles/index";
import { RemoveIcon, EditIcon } from "icons/index";

export interface IdeaProps {
  value: string;
  key: string;
}

export interface IdeaListProps {
  item: IdeaProps;
  onIdeaInputChange: (value: string, key: string) => void;
  removeIdea: (key: string) => void;
}

export const IdeaList = ({
  item,
  onIdeaInputChange,
  removeIdea,
}: IdeaListProps) => {
  var inputRef: TextInput | null;

  return (
    <View style={styles.ideaItem}>
      <View style={styles.ideaTextWrapper}>
        <TextInput
          ref={(input) => (inputRef = input)}
          onChangeText={(value) => onIdeaInputChange(value, item.key)}
          style={styles.ideaText}
          value={item.value}></TextInput>
      </View>
      <View style={styles.ideaIcons}>
        <Pressable
          onPress={() => {
            inputRef && inputRef.focus();
          }}
          style={Buttons.applyOpacity(styles.icon)}>
          <EditIcon width={20} height={20} />
        </Pressable>
        <Pressable
          onPress={() => removeIdea(item.key)}
          style={Buttons.applyOpacity(styles.icon)}>
          <RemoveIcon width={20} height={20} />
        </Pressable>
      </View>
    </View>
  );
};

const styles = StyleSheet.create({
  ideaItem: {
    overflow: "hidden",
    alignItems: "center",
    flexDirection: "row",
  },
  ideaTextWrapper: {
    borderTopWidth: Outlines.borderWidth.hairline,
    borderRadius: Outlines.borderRadius.small,
    width: "90%",
  },
  ideaText: {
    padding: Sizing.x10,
    flex: 1,
    ...Typography.body.x30,
  },
  ideaIcons: {
    alignItems: "center",
    width: "10%",
  },
  icon: {
    marginBottom: Sizing.x5,
  },
});
