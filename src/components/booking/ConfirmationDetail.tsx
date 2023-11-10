import * as React from "react"
import { View, StyleSheet } from "react-native"

import { Colors, Outlines, Sizing, Typography } from "styles/index"
import { EventLine, SectionDetail } from "common/interfaces/bookingInterface"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { appContext } from "contexts/contextApi"
import { useNavigation } from "@react-navigation/native"
import { fontWeight } from "../../styles/typography"

export const ConfirmationDetail = ({
  label,
  lineContent,
  isLastItem,
  callbackFn,
}: SectionDetail) => {
  const { colorScheme } = appContext()
  const navigation = useNavigation()
  const isLightMode = colorScheme === "light"

  if (lineContent == null) {
    return <></>
  }
  const onTextPress = (screen: string) => {
    if (callbackFn != null) navigation.navigate(screen)
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
        <>
          <View style={styles.headerContent} key={label}>
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
          {(lineContent as any).map((line: EventLine, index: number) =>
            line ? (
              <View style={styles.subHeaderContent} key={`${line.content}_${index}`}>
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
        </>
      ) : (
        <>
          <View style={styles.headerContent} key={label}>
            <SubHeaderText
              colors={[Colors.primary.s800, Colors.primary.neutral]}
              customStyle={{ marginRight: "auto", ...fontWeight.bold }}>
              {label}
            </SubHeaderText>
            {callbackFn ? (
              <SubHeaderText
                colors={[Colors.primary.s800, Colors.primary.s200]}
                customStyle={{ ...fontWeight.bold }}
                callbackFn={() => onTextPress(callbackFn.callbackFnScreen)}>
                {callbackFn.label}
              </SubHeaderText>
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
  text: {
    ...Typography.subHeader.x25,
    marginLeft: Sizing.x2,
  },
})
