import * as React from "react"
import {
  View,
  StyleSheet,
  Pressable,
  Text,
  LayoutChangeEvent,
  ImageBackground,
  Animated,
} from "react-native"

import { SafeAreaView } from "react-native-safe-area-context"
import {
  CameraIcon,
  LeftArrowIcon,
  PlaceholderIcon,
  RemoveIcon,
} from "assets/icons"
import { HeaderText } from "components/rnWrappers/headerText"
import { appContext, eventCreationContext } from "contexts/contextApi"
import { Buttons, Colors, Outlines, Sizing, Typography } from "styles/index"
import { EventCreationParamList } from "common/types/navigationTypes"
import { StackScreenProps } from "@react-navigation/stack"
import { SubHeaderText } from "components/rnWrappers/subHeaderText"
import { useMediaAccess } from "lib/hooks/useMediaAccess"
import { useCameraAccess } from "lib/hooks/useCameraAccess"
import { FullWidthButton } from "components/buttons/fullWidthButton"
import { fontWeight } from "../../../styles/typography"
import { applyOpacity } from "../../../styles/colors"

type Props = StackScreenProps<EventCreationParamList, "Image Cover Selection">

export const ImageCoverSelection = ({ navigation }: Props) => {
  const [layout, setLayout] = React.useState<any>(null)
  const [currImage, setCurrImage] = React.useState<string>("")
  const [currPhase, setCurrPhase] = React.useState<"selected" | "unselected">(
    "unselected"
  )
  const { launchImageLibrary, mediaObj, setMediaObj } = useMediaAccess()
  const { launchCamera, imageObj, setImgObj } = useCameraAccess()
  const { colorScheme } = appContext()
  const { setImageUri, imageURI } = eventCreationContext()

  const mainPositionAnimation = React.useRef<any>(new Animated.Value(0)).current
  const secondPositionAnimation = React.useRef<any>(
    new Animated.Value(200)
  ).current
  const isLightMode = colorScheme === "light"

  React.useEffect(() => {
    if (!currImage && imageURI) {
      animateNavigationButtons()
      setCurrImage(imageURI)
    }

    if (imageObj?.assets[0]?.uri && imageObj !== currImage) {
      setCurrImage(imageObj.assets[0].uri)
      setCurrPhase("selected")
      animateNavigationButtons()
    } else if (mediaObj?.assets[0]?.uri && mediaObj !== currImage) {
      setCurrImage(mediaObj.assets[0].uri)
      setCurrPhase("selected")
      animateNavigationButtons()
    }

    setMediaObj(null)
    setImgObj(null)
  }, [imageObj, mediaObj])

  React.useEffect(() => {
    if (!currImage && currPhase === "selected") {
      animateNavigationButtons()
    }
  }, [currImage])

  const onBackNavigationPress = () => {
    navigation.goBack()
  }

  const onNextPress = () => {
    if (currImage) {
      setImageUri(currImage)
    }
    navigation.navigate("Event Card Customization")
  }
  const onLayout = (e: LayoutChangeEvent) => setLayout(e.nativeEvent.layout)
  const onRemoveImage = () => {
    setCurrImage("")
    setImageUri("")
  }

  const animateNavigationButtons = (phase?: "selected" | "unselected") => {
    if (phase) setCurrPhase(phase)
    Animated.parallel([
      Animated.timing(mainPositionAnimation, {
        toValue: (mainPositionAnimation as any)._value === 0 ? 200 : 0,
        useNativeDriver: false,
        duration: 200,
      }),
      Animated.timing(secondPositionAnimation, {
        toValue: (secondPositionAnimation as any)._value === 200 ? 0 : 200,
        useNativeDriver: false,
        duration: 200,
      }),
    ]).start()
  }

  return (
    <SafeAreaView
      style={[
        styles.safeArea,
        {
          backgroundColor: isLightMode
            ? Colors.primary.neutral
            : Colors.neutral.s600,
        },
      ]}>
      <View style={{ width: "100%", height: "100%", alignItems: "center" }}>
        <View style={styles.navigation}>
          <Pressable onPress={onBackNavigationPress} hitSlop={10}>
            <LeftArrowIcon
              width={24}
              height={24}
              color={isLightMode ? Colors.primary.s600 : Colors.primary.neutral}
            />
          </Pressable>
        </View>
        <View style={styles.header}>
          <HeaderText
            customStyles={{ marginBottom: Sizing.x10 }}
            colorScheme={colorScheme}>
            Select a cover image
          </HeaderText>
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            Upload a cover image to draw attention to your event card.
          </SubHeaderText>
        </View>
        <View style={styles.main}>
          <View
            style={[
              styles.imagePreview,
              { paddingBottom: layout ? layout.height : 0 },
            ]}>
            {!currImage && layout ? (
              <View style={styles.imagePreview_inner}>
                <PlaceholderIcon
                  width={Sizing.x80}
                  height={Sizing.x80}
                  strokeWidth={1}
                  stroke={Colors.neutral.s100}
                />
              </View>
            ) : (
              <ImageBackground
                style={{
                  width: "100%",
                  height: "105%",
                  position: "absolute",
                  top: 0,
                }}
                source={{
                  uri: currImage,
                }}
                imageStyle={styles.imagePreview_image}
                resizeMode="cover">
                {currImage ? (
                  <View
                    style={[
                      styles.removeButtonWrapper,
                      {
                        backgroundColor: applyOpacity(Colors.neutral.s800, 0.5),
                      },
                    ]}>
                    <Pressable
                      style={Buttons.applyOpacity(styles.removeButton)}
                      onPress={onRemoveImage}>
                      <RemoveIcon
                        style={styles.icon}
                        strokeWidth={2.5}
                        stroke={Colors.primary.s800}
                        fill={Colors.primary.s800}
                      />
                    </Pressable>
                  </View>
                ) : (
                  <></>
                )}
              </ImageBackground>
            )}
          </View>
          <View
            onLayout={onLayout}
            style={[
              styles.main_navigation,
              {
                backgroundColor: isLightMode
                  ? Colors.primary.neutral
                  : Colors.neutral.s600,
              },
            ]}>
            <Animated.View
              style={[
                styles.main_navigation_wrapper,
                {
                  transform: [
                    {
                      translateY: mainPositionAnimation,
                    },
                  ],
                },
              ]}>
              <View style={styles.main_navigation_wrapper_top}>
                <Pressable
                  style={Buttons.applyOpacity(styles.media_button)}
                  onPress={launchImageLibrary}>
                  <Text style={styles.button_text}>Gallery</Text>
                  <PlaceholderIcon
                    width={Sizing.x25}
                    height={Sizing.x25}
                    strokeWidth={2}
                    stroke={Colors.primary.s800}
                  />
                </Pressable>
                <Pressable
                  style={Buttons.applyOpacity(styles.media_button)}
                  onPress={launchCamera}>
                  <Text style={styles.button_text}>Take photo</Text>
                  <CameraIcon
                    width={Sizing.x25}
                    height={Sizing.x25}
                    strokeWidth={2}
                    stroke={Colors.primary.s800}
                  />
                </Pressable>
              </View>
              <View style={styles.main_navigation_wrapper_bottom}>
                <FullWidthButton
                  onPressCallback={onNextPress}
                  text={"Skip Step"}
                  lightMode={true}
                />
              </View>
            </Animated.View>
            <Animated.View
              style={[
                styles.second_navigation_wrapper,
                {
                  transform: [
                    {
                      translateY: secondPositionAnimation,
                    },
                  ],
                },
              ]}>
              <FullWidthButton
                onPressCallback={() => animateNavigationButtons("unselected")}
                text={"Change"}
                lightMode={true}
                buttonType={"transparent"}
              />
              <FullWidthButton
                onPressCallback={onNextPress}
                text={"Continue"}
                lightMode={true}
              />
            </Animated.View>
          </View>
        </View>
      </View>
    </SafeAreaView>
  )
}

const styles = StyleSheet.create({
  safeArea: {
    flex: 1,
    alignItems: "center",
  },
  navigation: {
    marginVertical: Sizing.x15,
    alignSelf: "center",
    width: "90%",
  },
  header: {
    alignSelf: "center",
    width: "90%",
    marginBottom: Sizing.x15,
  },
  main: {
    flex: 1,
    width: "100%",
    alignItems: "center",
  },
  imagePreview: {
    width: "100%",
    height: "100%",
    justifyContent: "center",
    overflow: "hidden",
    backgroundColor: Colors.applyOpacity("#0F4871", 0.5),
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  imagePreview_inner: {
    alignItems: "center",
    justifyContent: "center",
    alignSelf: "center",
    width: "90%",
    height: "90%",
    borderColor: Colors.neutral.s100,
    borderWidth: Sizing.x5,
    borderRadius: 10,
    borderStyle: "dashed",
  },
  imagePreview_image: {
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  main_navigation: {
    position: "absolute",
    bottom: 0,
    height: Sizing.x110,
    width: "100%",
    alignItems: "center",
    overflow: "hidden",
    justifyContent: "space-evenly",
    borderTopLeftRadius: Outlines.borderRadius.large,
    borderTopRightRadius: Outlines.borderRadius.large,
  },
  main_navigation_wrapper: {
    position: "absolute",
    bottom: 0,
    width: "100%",
    height: "100%",
    alignItems: "center",
    justifyContent: "space-evenly",
  },
  main_navigation_wrapper_top: {
    width: "90%",
    flexDirection: "row",
    justifyContent: "space-between",
  },
  main_navigation_wrapper_bottom: {
    width: "90%",
    marginBottom: Sizing.x15,
  },
  second_navigation_wrapper: {
    position: "absolute",
    bottom: 0,
    width: "90%",
    height: "100%",
    alignItems: "center",
    justifyContent: "space-evenly",
  },
  media_button: {
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    width: "45%",
    paddingVertical: Sizing.x8,
    backgroundColor: Colors.primary.s400,
    borderWidth: Outlines.borderWidth.base,
    borderColor: Colors.primary.s800,
    borderRadius: Outlines.borderRadius.base,
    ...Outlines.shadow.base,
  },
  button_text: {
    ...Typography.subHeader.x30,
    ...fontWeight.semibold,
    color: Colors.primary.s800,
    marginRight: Sizing.x5,
  },
  removeButtonWrapper: {
    position: "absolute",
    top: Sizing.x10,
    right: Sizing.x10,
    width: Sizing.x50,
    height: Sizing.x50,
    borderRadius: Outlines.borderRadius.max,
    alignItems: "center",
    justifyContent: "center",
  },
  removeButton: {
    width: Sizing.x40,
    height: Sizing.x40,
    borderRadius: Outlines.borderRadius.max,
    backgroundColor: Colors.primary.neutral,
    alignItems: "center",
    justifyContent: "center",
  },
  icon: {
    width: Sizing.x25,
    height: Sizing.x25,
  },
})
