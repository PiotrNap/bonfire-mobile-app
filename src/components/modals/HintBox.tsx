import * as React from "react"
import { View, Text, StyleSheet, Pressable } from "react-native"

import { BodyText } from "components/rnWrappers/bodyText"
import { Colors, Outlines, Sizing } from "styles/index"
import { fontWeight } from "../../styles/typography"
import { RemoveIcon } from "assets/icons"

interface Props {
  text: string
  closeable: boolean
  closeCallback: () => {}
}

export const HintBox = ({ text, closeable, closeCallback }: Props) => {
  const [isOpen, setIsOpen] = React.useState<boolean>(true)

  const closeHintBox = async () => {
    closeCallback()
    setIsOpen(false)
  }

  return isOpen ? (
    <View style={styles.container}>
      <BodyText
        customStyle={{
          ...fontWeight.semibold,
          fontSize: Sizing.x14,
          width: "90%",
        }}>
        <Text style={{ ...fontWeight.bold }}>Hint:</Text> {text}
      </BodyText>
      {closeable && (
        <Pressable
          style={styles.closeButton}
          hitSlop={Sizing.x2}
          onPress={closeHintBox}>
          <RemoveIcon style={styles.closeIcon} strokeWidth={2.4} />
        </Pressable>
      )}
    </View>
  ) : (
    <></>
  )
}

const styles = StyleSheet.create({
  container: {
    justifyContent: "space-between",
    width: "90%",
    marginLeft: "auto",
    marginRight: "auto",
    flexDirection: "row",
    padding: Sizing.x8,
    borderWidth: Outlines.borderWidth.base,
    borderColor: Colors.primary.s300,
    backgroundColor: Colors.primary.s180,
    borderRadius: Outlines.borderRadius.base,
  },
  closeButton: {
    alignItems: "center",
    justifyContent: "center",
    width: Sizing.x25,
    height: Sizing.x25,
  },
  closeIcon: {
    width: Sizing.x20,
    height: Sizing.x20,
    color: Colors.primary.s800,
  },
})
