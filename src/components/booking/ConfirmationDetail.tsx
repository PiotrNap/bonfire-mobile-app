import * as React from "react"
import { View, StyleSheet, Pressable } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { EventLine, SectionDetail } from "common/interfaces/bookingInterface"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { useNavigation } from "@react-navigation/native"
import { fontWeight } from "../../styles/typography"
import { CopyMessage } from "components/popups/copyMessage"
import { getRandomKey } from "lib/utils"

export const ConfirmationDetail = ({
  label,
  lineContent,
  isLastItem,
  callbackFn,
}: SectionDetail) => {
  const [isCopyPopupVisible, setIsCopyPopupVisible] = React.useState<boolean>(false)
  const { colorScheme } = appContext()
  const navigation = useNavigation()
  const isLightMode = colorScheme === "light"

  if (lineContent == null) {
    return <></>
  }
  const onTextPress = (screen: string) => {
    if (callbackFn != null) navigation.navigate(screen)
  }
  const onCallbackIconPress = () => {
    setIsCopyPopupVisible(true)
    callbackFn?.onPress()
    setTimeout(() => setIsCopyPopupVisible(false), 1000)
  }
  return (
    <View
      style={[
        styles.container,
        !isLastItem && {
          borderBottomWidth: Outlines.borderWidth.base,
          borderBottomColor: isLightMode ? Colors.primary.s350 : Colors.primary.s300,
        },
      ]}>
      {Array.isArray(lineContent) ? (
        <View key={getRandomKey(4)}>
          <View style={styles.headerContent}>
            <SubHeaderText
              children={label}
              colors={[Colors.primary.s800, Colors.primary.neutral]}
              customStyle={{ marginRight: "auto", ...fontWeight.bold }}
            />
            {callbackFn ? (
              <SubHeaderText
                customStyle={{ ...fontWeight.bold }}
                children={callbackFn.label}
                colors={[Colors.primary.s800, Colors.primary.s200]}
                callbackFn={() => onTextPress(callbackFn.callbackFnScreen)}
              />
            ) : (
              <></>
            )}
          </View>
          {lineContent.map((line: EventLine, index: number) =>
            line ? (
              <View style={styles.subHeaderContent} key={getRandomKey(4)}>
                {line.icon}
                <SubHeaderText
                  children={line.content}
                  colors={[Colors.primary.s800, Colors.primary.neutral]}
                  customStyle={styles.text}
                />
              </View>
            ) : (
              <></>
            )
          )}
        </View>
      ) : (
        <>
          <View style={styles.headerContent} key={label}>
            <SubHeaderText
              colors={[Colors.primary.s800, Colors.primary.neutral]}
              customStyle={{ marginRight: "auto", ...fontWeight.bold }}>
              {label}
            </SubHeaderText>
            {callbackFn ? (
              <>
                <SubHeaderText
                  colors={[Colors.primary.s800, Colors.primary.s200]}
                  customStyle={{ ...fontWeight.bold }}
                  callbackFn={() =>
                    callbackFn?.onPress
                      ? callbackFn?.onPress
                      : onTextPress(callbackFn.callbackFnScreen)
                  }>
                  {callbackFn.label}
                </SubHeaderText>
              </>
            ) : (
              <></>
            )}
          </View>
          <View style={styles.subHeaderContent} key={lineContent.content}>
            {lineContent.icon}
            <SubHeaderText
              colors={[Colors.primary.s800, Colors.primary.neutral]}
              customStyle={styles.text}>
              {lineContent.content}
            </SubHeaderText>
            {callbackFn?.icon ? (
              <>
                <Pressable
                  style={styles.copyCallbackButton}
                  hitSlop={10}
                  onPress={onCallbackIconPress}>
                  {callbackFn?.icon}
                </Pressable>
                <CopyMessage isActive={isCopyPopupVisible} />
              </>
            ) : (
              <></>
            )}
          </View>
        </>
      )}
    </View>
  )
}

const styles = StyleSheet.create({
  container: {
    marginHorizontal: Sizing.x5,
    paddingVertical: Sizing.x15,
  },
  headerContent: {
    flexDirection: "row",
    justifyContent: "space-between",
    width: "100%",
    marginBottom: 4,
  },
  subHeaderContent: {
    flexDirection: "row",
    width: "100%",
    marginBottom: Sizing.x5,
  },
  copyCallbackButton: {
    flexDirection: "row",
    margin: Sizing.x2,
    zIndex: 10,
  },
  text: {
    ...Typography.subHeader.x25,
    marginLeft: Sizing.x2,
  },
})
