import * as React from "react"
import { View, StyleSheet, Pressable, Linking, Text } from "react-native"

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
  const onTextPress = (screen: string, param?: any) => {
    if (callbackFn != null) navigation.navigate(screen, param)
  }
  const onCallbackIconPress = () => {
    setIsCopyPopupVisible(true)
    callbackFn?.onPress()
    setTimeout(() => setIsCopyPopupVisible(false), 1000)
  }

  const ensureProtocol = (url) => {
    if (!url.startsWith("http://") && !url.startsWith("https://")) {
      return "http://" + url // Default to http if no protocol is specified
    }
    return url
  }

  const detectAndWrapUrls = (inputText: any) => {
    if (!inputText || typeof inputText !== "string") return ""
    const urlRegex = /\b(?:https?:\/\/|www\.)[^\s$.?#].[^\s]*\b/g
    const urls = inputText.match(urlRegex)

    if (!urls) {
      return <Text>{inputText}</Text>
    }

    const parts = inputText.split(urlRegex).filter((part) => part)
    const processedParts: React.ReactNode[] = []

    parts.forEach((part, index) => {
      processedParts.push(<Text key={`text-${index}`}>{part}</Text>)

      if (urls[index]) {
        const urlWithProtocol = ensureProtocol(urls[index])
        processedParts.push(
          <Text
            key={`url-${index}`}
            style={{ color: isLightMode ? "#1338BE" : "#89CFF0" }}
            onPress={() => Linking.openURL(urlWithProtocol)}>
            {urls[index]}
          </Text>
        )
      }
    })

    return <Text>{processedParts}</Text>
  }

  return (
    <View
      key={getRandomKey(4)}
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
                callbackFn={() =>
                  onTextPress(callbackFn.callbackFnScreen, callbackFn?.param)
                }
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
                      : onTextPress(callbackFn.callbackFnScreen, callbackFn?.param)
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
              {label === "Note"
                ? detectAndWrapUrls(lineContent.content)
                : lineContent.content}
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
