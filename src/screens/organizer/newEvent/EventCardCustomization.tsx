import * as React from "react";
import {
  View,
  StyleSheet,
  Pressable,
  Switch,
  ScrollView,
  Text,
} from "react-native";

import { StackScreenProps } from "@react-navigation/stack";
import { EventCreationParamList } from "common/types/navigationTypes";
import { appContext, eventCreationContext } from "contexts/contextApi";
import { SafeAreaView } from "react-native-safe-area-context";
import { Colors, Sizing, Outlines, Buttons, Typography } from "styles/index";
import { LeftArrowIcon } from "assets/icons";
import { HeaderText } from "components/rnWrappers/headerText";
import { SubHeaderText } from "components/rnWrappers/subHeaderText";
import { EventsListCard } from "components/booking/EventsListCard";
import {
  SliderHuePicker,
  SliderSaturationPicker,
  SliderValuePicker,
} from "react-native-slider-color-picker";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { BodyText } from "components/rnWrappers/bodyText";
import { applyOpacity } from "../../../styles/colors";
import tinyColor from "tinycolor2";
import Slider from "react-native-smooth-slider";
import { fontWeight } from "../../../styles/typography";

type Props = StackScreenProps<
  EventCreationParamList,
  "Event Card Customization"
>;
type SliderColor = { a: number; h: number; s: number; v: number };
type SliderType = "saturation" | "hue" | "value" | "opacity";

export const EventCardCustomization = ({ navigation }: Props) => {
  const {
    textContent,
    imageURI,
    fromDate,
    toDate,
    setEventCardColor,
    setEventTitleColor,
    eventCardColor,
  } = eventCreationContext();
  const { colorScheme } = appContext();
  const [bgColor, setBgColor] = React.useState<any>({
    a: 1,
    h: 0,
    s: 0.01,
    v: 0.01,
  });
  const [titleColor, setTitleColor] = React.useState<any>({
    a: 1,
    h: 0,
    s: 0.01,
    v: 1,
  });
  const [currColor, setCurrColor] = React.useState<SliderColor>({
    a: 0,
    h: 0,
    s: 0,
    v: 0,
  });
  const [opacity, setOpacity] = React.useState<number>(0);
  const [titleOpacity, setTitleOpacity] = React.useState<number>(1);
  const [transparent, setTransparent] = React.useState<boolean>(false);
  const [activeSelection, setActiveSelection] = React.useState<
    "background" | "title"
  >("background");
  const _color = tinyColor(bgColor).setAlpha(opacity).toRgbString();
  const _titleColor = tinyColor(titleColor)
    .setAlpha(titleOpacity)
    .toRgbString();

  React.useEffect(() => {
    if (eventCardColor) setBgColor(eventCardColor);
  }, []);

  const toHexString = (value: any) => tinyColor(value).toHexString();
  const toHsvString = (value: any) => tinyColor(value).toHsvString();

  // const currColor = activeSelection === "background" ? color : titleColor;
  const isLightMode = colorScheme !== "dark";
  const buttonStyle = React.useCallback(
    (type) => {
      var bgColor = isLightMode
        ? type === activeSelection
          ? Colors.primary.s800
          : Colors.primary.s200
        : type === activeSelection
        ? Colors.primary.neutral
        : "transparent";
      var bdColor = isLightMode ? "transparent" : Colors.primary.neutral;
      var txColor = isLightMode
        ? type === activeSelection
          ? Colors.primary.neutral
          : Colors.primary.s800
        : type === activeSelection
        ? Colors.primary.s800
        : Colors.primary.neutral;

      return { backgroundColor: bgColor, borderColor: bdColor, color: txColor };
    },
    [activeSelection, colorScheme]
  );

  const onColorChange = (
    colorHsvOrRgb: SliderColor,
    resType: string,
    sliderType: SliderType
  ) => {
    if (resType === "end") {
      const { h, v, s } = colorHsvOrRgb;
      let newColor = currColor;

      switch (sliderType) {
        case "hue": {
          setCurrColor({ ...currColor, h });
          newColor = { ...currColor, h };
          break;
        }
        case "value": {
          setCurrColor({ ...currColor, v });
          newColor = { ...currColor, v };
          break;
        }
        case "saturation": {
          setCurrColor({ ...currColor, s });
          newColor = { ...currColor, s };
          break;
        }
        default:
          console.log("Slider type not recognized");
      }

      if (activeSelection === "background") {
        setBgColor(newColor);
      } else {
        setTitleColor(newColor);
      }
    }
  };
  const onOpacityChange = (val: number) => {
    if (activeSelection === "background") {
      setOpacity(val);
    } else {
      setTitleOpacity(val);
    }
  };
  const onBackNavigationPress = () => navigation.goBack();
  const onNextPress = () => {
    setEventCardColor(transparent ? "transparent" : _color);
    setEventTitleColor(transparent ? "white" : _titleColor);
    navigation.navigate("Event Confirmation Details", {
      isNewEvent: true,
    });
  };
  const onBackgroundSelected = () => {
    setCurrColor(toHsvString(bgColor));
    setActiveSelection("background");
  };
  const onTitleSelected = () => {
    setCurrColor(toHsvString(titleColor));
    setActiveSelection("title");
  };
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
      <ScrollView
        contentContainerStyle={{ alignItems: "center" }}
        style={{ width: "100%", height: "100%" }}>
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
            Customize your event card
          </HeaderText>
          <SubHeaderText colors={[Colors.primary.s800, Colors.primary.neutral]}>
            Give your card some colors to organize your events ex. by time,
            theme or to simply bring aesthetics to your events.
          </SubHeaderText>
        </View>
        <View style={styles.selectionButtonsWrapper}>
          <Pressable
            onPress={onBackgroundSelected}
            style={Buttons.applyOpacity(
              Object.assign(
                {},
                styles.colorSelectionButton,
                buttonStyle("background")
              )
            )}>
            <Text
              style={[
                styles.colorSelectionButtonText,
                { color: buttonStyle("background").color },
              ]}>
              Background
            </Text>
          </Pressable>
          <Pressable
            onPress={onTitleSelected}
            style={Buttons.applyOpacity(
              Object.assign(
                {},
                styles.colorSelectionButton,
                buttonStyle("title")
              )
            )}>
            <Text
              style={[
                styles.colorSelectionButtonText,
                { color: buttonStyle("title").color },
              ]}>
              Title
            </Text>
          </Pressable>
        </View>
        <View style={styles.colorPickerWrapper}>
          <SliderHuePicker
            oldColor={toHexString({
              h: currColor.h,
              s: currColor.s,
              v: 1,
              a: 1,
            })}
            trackStyle={[{ height: Sizing.x10, width: "100%" }]}
            thumbStyle={styles.thumb}
            onColorChange={(val: SliderColor, res: string) =>
              onColorChange(val, res, "hue")
            }
            useNativeDriver={false}
          />
        </View>
        <View style={styles.colorPickerWrapper}>
          <SliderSaturationPicker
            oldColor={toHexString({
              h: currColor.h,
              s: currColor.s,
              v: 1,
              a: 1,
            })}
            trackStyle={[{ height: Sizing.x10, width: "100%" }]}
            thumbStyle={styles.thumb}
            onColorChange={(val: SliderColor, res: string) =>
              onColorChange(val, res, "saturation")
            }
            useNativeDriver={false}
            style={[
              styles.colorPicker,
              {
                backgroundColor: tinyColor({
                  h: currColor.h,
                  s: 1,
                  v: 1,
                }).toHexString(),
              },
            ]}
          />
        </View>
        <View style={styles.colorPickerWrapper}>
          <SliderValuePicker
            oldColor={toHexString({
              h: currColor.h,
              s: currColor.s,
              v: currColor.v,
              a: 1,
            })}
            minumumValue={0.2}
            step={0.05}
            trackStyle={[{ height: Sizing.x10, width: "100%" }]}
            thumbStyle={styles.thumb}
            onColorChange={(val: SliderColor, res: string) =>
              onColorChange(val, res, "value")
            }
            trackImage={require("react-native-slider-color-picker/brightness_mask.png")}
            useNativeDriver={false}
            style={[
              styles.colorPicker,
              {
                backgroundColor: "black",
              },
            ]}
          />
        </View>
        <View style={styles.colorPickerWrapper}>
          <View
            style={{
              backgroundColor: "transparent",
              flex: 1,
              alignItems: "stretch",
              justifyContent: "center",
            }}>
            <Slider
              value={activeSelection === "background" ? opacity : titleOpacity}
              minumumValue={0.1}
              maximumValue={1}
              minimumTrackTintColor={"#3f3f3f"}
              maximumTrackTintColor={"#b3b3b3"}
              thumbStyle={styles.thumb}
              trackStyle={[
                {
                  height: Sizing.x10,
                  width: "100%",
                  borderRadius: Sizing.x10 / 2,
                },
              ]}
              useNativeDriver={false}
              style={[styles.colorPicker]}
              trackImage={require("../../../assets/images/gradient_2.png")}
              onValueChange={onOpacityChange}
              onSlidingComplete={onOpacityChange}
            />
          </View>
        </View>
        <View style={styles.enableColorsWrapper}>
          <BodyText
            customStyle={{ ...fontWeight.semibold, marginRight: Sizing.x5 }}
            colors={[Colors.primary.s800, Colors.primary.neutral]}>
            Colors {!transparent ? "enabled" : "disabled"}
          </BodyText>
          <Switch
            trackColor={{
              false: Colors.neutral.s200,
              true: Colors.neutral.s800,
            }}
            thumbColor={
              !isLightMode ? Colors.primary.s800 : Colors.primary.neutral
            }
            ios_backgroundColor={Colors.primary.brand}
            onValueChange={() => setTransparent((prev) => !prev)}
            value={!transparent}
          />
        </View>
        <View style={styles.main}>
          {fromDate && toDate && (
            <EventsListCard
              isEventCardPreview={true}
              title={textContent.title}
              description={textContent.description}
              fromDate={fromDate}
              toDate={toDate}
              image={imageURI}
              color={applyOpacity(toHexString(bgColor), opacity)}
              titleColor={applyOpacity(toHexString(titleColor), titleOpacity)}
              isTransparent={transparent}
            />
          )}
        </View>
        <FullWidthButton
          text="Next"
          disabled={false}
          style={{ width: "90%", marginBottom: Sizing.x15 }}
          colorScheme={colorScheme}
          onPressCallback={onNextPress}
        />
      </ScrollView>
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
    alignSelf: "center",
    width: "90%",
  },
  header: {
    alignSelf: "center",
    width: "90%",
    marginBottom: Sizing.x15,
  },
  subHeader: {
    alignSelf: "center",
    width: "90%",
    marginTop: Sizing.x5,
  },
  main: {
    width: "90%",
  },
  currentColor: {
    width: 80,
    height: 40,
    borderRadius: 20,
    marginBottom: 24,
    borderWidth: 2,
    borderColor: "white",
  },
  colorPickerWrapper: {
    height: Sizing.x30,
    width: "90%",
    marginTop: Sizing.x5,
  },
  colorPicker: {
    height: Sizing.x10,
    borderRadius: Outlines.borderRadius.base,
  },
  thumb: {
    width: Sizing.x25,
    height: Sizing.x25,
    borderColor: "white",
    borderWidth: 1,
    borderRadius: Outlines.borderRadius.max,
    shadowColor: "black",
    shadowOffset: {
      width: 0,
      height: 2,
    },
    shadowRadius: 2,
    shadowOpacity: 0.35,
  },
  enableColorsWrapper: {
    width: "90%",
    marginVertical: Sizing.x5,
    flexDirection: "row",
    justifyContent: "flex-end",
  },
  customizingButtonsWrapper: {
    flexDirection: "row",
  },
  selectionButtonsWrapper: {
    width: "90%",
    alignItems: "center",
    justifyContent: "space-between",
    flexDirection: "row",
    marginTop: Sizing.x10,
    marginBottom: Sizing.x5,
  },
  customizingButton: {
    width: Sizing.x30,
  },
  colorSelectionButton: {
    borderRadius: Outlines.borderRadius.base,
    borderWidth: Outlines.borderWidth.base,
    flex: 1,
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "center",
    width: "30%",
    paddingVertical: Sizing.x5,
    paddingHorizontal: Sizing.x10,
    marginHorizontal: Sizing.x10,
    ...Outlines.shadow.base,
  },
  colorSelectionButtonText: {
    ...Typography.header.x20,
    marginRight: Sizing.x5,
  },
});
