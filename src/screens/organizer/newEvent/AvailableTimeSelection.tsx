import * as React from "react";
import { Pressable, StyleSheet, View, Text } from "react-native";

import { CurvedArrow, LeftArrowIcon, PlusIcon } from "assets/icons";
import { FullWidthButton } from "components/buttons/fullWidthButton";
import { AvailabilityList } from "components/events/availabilityList";
import { PlainInputPicker } from "components/forms/PlainInputPicker";
import { TimePickerInput } from "components/forms/TimePickerInput";
import { HeaderText } from "components/rnWrappers/headerText";
import { appContext, eventCreationContext } from "contexts/contextApi";
import { SafeAreaView } from "react-native-safe-area-context";
import {
  Buttons,
  Colors,
  Forms,
  Outlines,
  Sizing,
  Typography,
} from "styles/index";
import { StackScreenProps } from "@react-navigation/stack";
import { EventCreationParamList } from "common/types/navigationTypes";
import { roundDateMinutes } from "lib/utils";
import { fontWeight } from "../../../styles/typography";

type Props = StackScreenProps<
  EventCreationParamList,
  "Available Time Selection"
>;

export const AvailableTimeSelection = ({ navigation }: Props) => {
  const [fromTime, setFromTime] = React.useState<Date>(
    roundDateMinutes(new Date())
  );
  const [toTime, setToTime] = React.useState<Date>(
    roundDateMinutes(new Date())
  );
  const [minTime, setMinTime] = React.useState<number>(0);
  const [maxTime, setMaxTime] = React.useState<number>(0);
  const [maxInputRange, setMaxInputRange] = React.useState<any[]>([]);
  const [minInputRange, setMinInputRange] = React.useState<any[]>([]);
  const [enabledPicker, setEnabledPicker] = React.useState<boolean>(true);
  const [openPicker, setOpenPicker] = React.useState<string | null>(null);
  const [alreadyCreated, setAlreadyCreated] = React.useState<boolean>(false);
  const { colorScheme } = appContext();
  const { addAvailability, availabilities } = eventCreationContext();

  React.useEffect(() => {
    setMinInputRange(calculateMinRange());
    setMaxInputRange(calculateMaxRange());
  }, [fromTime, toTime, minTime, maxTime]);

  const calculateMinRange = () => {
    let arr: any[] = [];
    const range = toTime.getTime() - fromTime.getTime();

    if (range > 0) {
      // in range of 5min
      for (let i = 5; i < range / 1000 / 60 + 1; i += 5) {
        arr.push(i);
      }
      if (!minTime) setMinTime(arr[0]);
      setEnabledPicker(true);
    } else {
      arr = ["--"];
      setMinTime(0);
      setEnabledPicker(false);
    }

    return arr;
  };

  const calculateMaxRange = () => {
    let arr: any[] = [];
    const range = toTime.getTime() - fromTime.getTime();

    if (range > 0) {
      for (let i = 5; i < range / 1000 / 60 + 1; i += 5) {
        arr.push(i);
      }
      if (!maxTime) setMaxTime(arr[0]);
      setEnabledPicker(true);

      // when the `maxTime` is bigger than new selected time range
      if (maxTime > arr[arr.length - 1]) setMaxTime(arr[arr.length - 1]);
    } else {
      arr = ["--"];
      setMaxTime(0);
      setEnabledPicker(false);
    }

    // when minTime is already selected, substract it from maxTime,
    if (minTime) arr.splice(0, arr.indexOf(minTime));

    return arr;
  };

  const hasExistingAvailability = !!availabilities.find(
    (el) =>
      el &&
      el.from === fromTime &&
      el.to === toTime &&
      el.maxDuration === maxTime &&
      el.minDuration === minTime
  );
  const hasAvailabilities = !!availabilities.length;

  const isLightMode = colorScheme === "light";
  const isDisabledButton = !hasAvailabilities;
  const isDisabledAddBtn =
    String(fromTime) === String(toTime) || String(fromTime) > String(toTime);

  const onTimeChangeValue = (label: string, val: Date) => {
    if (label === "From") setFromTime(val);
    if (label === "To") setToTime(val);
  };
  const onMinValueChange = (val: number) => {
    setMinTime(val != 0 ? val : 1);
    // if new min. time slot > max. time slot, reset maxTime
    if (val > maxTime) setMaxTime(val);
  };
  const onMaxValueChange = (val: number) => setMaxTime(val);
  const onOpenChange = (label: string | null) => setOpenPicker(label);
  const addNewAvailability = () => {
    addAvailability({
      from: fromTime,
      to: toTime,
      maxDuration: maxTime ?? maxInputRange[0],
      minDuration: minTime ?? minInputRange[0],
    });
    // react-navigation doesn't update screen immediately after context dispatch
    navigation.setParams({ availabilities: availabilities.length++ });

    setAlreadyCreated(true);
  };
  /**
   * Navigation handlers
   */
  const onBackNavigationPress = () => navigation.goBack();
  const onNextPress = () => navigation.navigate("Image Cover Selection");

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
      <View style={{ height: "100%", width: "90%", alignItems: "center" }}>
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
        <View style={styles.timePickersWrapper}>
          <View style={styles.timeInputWrapper}>
            <TimePickerInput
              onValueChange={onTimeChangeValue}
              timeValue={fromTime}
              label="From"
              openPicker={openPicker}
              onOpenChange={onOpenChange}
            />
          </View>
          <View style={styles.timeInputWrapper}>
            <TimePickerInput
              onValueChange={onTimeChangeValue}
              timeValue={toTime}
              label="To"
              openPicker={openPicker}
              onOpenChange={onOpenChange}
            />
          </View>
        </View>
        <View style={styles.timeSlotsPickersWrapper}>
          <View style={styles.slotsWrapperTop}>
            <PlainInputPicker
              label="Min. duration"
              minTime={minTime}
              inputRange={minInputRange}
              onValueChange={onMinValueChange}
              openPicker={openPicker}
              onOpenChange={onOpenChange}
              enabledPicker={enabledPicker}
            />
          </View>
          <View style={styles.slotsWrapperBottom}>
            <PlainInputPicker
              label="Max. duration"
              maxTime={maxTime}
              inputRange={maxInputRange}
              onValueChange={onMaxValueChange}
              openPicker={openPicker}
              onOpenChange={onOpenChange}
              enabledPicker={enabledPicker}
            />
          </View>
        </View>
        <View style={styles.listHeaderWrapper}>
          <Text
            style={[
              isLightMode
                ? { color: Colors.primary.s800 }
                : { color: Colors.primary.neutral },
              styles.listHeaderText,
            ]}>
            Total ({availabilities.length})
          </Text>
          <View style={{ flexDirection: "row" }}>
            <Text
              style={[
                isLightMode
                  ? { color: Colors.primary.s800 }
                  : { color: Colors.primary.neutral },
                styles.listHeaderText,
              ]}></Text>
            <Pressable
              onPress={addNewAvailability}
              disabled={isDisabledAddBtn}
              style={Buttons.applyOpacity(
                Object.assign(
                  {},
                  styles.addBtn,
                  isDisabledAddBtn
                    ? { backgroundColor: Colors.neutral.s200 }
                    : {}
                )
              )}>
              <PlusIcon
                style={[
                  styles.plusIcon,
                  {
                    //@ts-ignore
                    color: isDisabledAddBtn
                      ? Colors.neutral.s500
                      : Colors.primary.s200,
                  },
                ]}
                strokeWidth={3}
              />
            </Pressable>
          </View>
        </View>
        {hasAvailabilities ? (
          <AvailabilityList />
        ) : !hasAvailabilities && !alreadyCreated ? (
          <View
            style={{
              flexDirection: "row",
              marginLeft: "auto",
              marginRight: Sizing.x20,
            }}>
            <Text style={{ ...fontWeight.bold, textAlign: "center" }}>
              Select your time frame &{"\n"} add new time slot
            </Text>
            <CurvedArrow
              style={[
                styles.arrowIcon,
                //@ts-ignore
                { fill: Colors.primary.s350 },
              ]}
            />
          </View>
        ) : null}
        <FullWidthButton
          text="Next"
          disabled={isDisabledButton}
          style={{ marginTop: "auto", marginBottom: Sizing.x15 }}
          colorScheme={colorScheme}
          onPressCallback={onNextPress}
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
  },
  label_light: {
    ...Forms.inputLabel.primary_light,
  },
  label_dark: {
    ...Forms.inputLabel.primary_dark,
  },
  timePickersWrapper: {
    marginTop: Sizing.x10,
    width: "100%",
    justifyContent: "space-between",
    flexDirection: "row",
    zIndex: 5,
  },
  timeInputWrapper: {
    width: "45%",
    flexDirection: "column",
    zIndex: 10,
  },
  timeSlotsPickersWrapper: {
    zIndex: 1,
    width: "100%",
  },
  slotsWrapperTop: {
    zIndex: 6,
  },
  slotsWrapperBottom: {
    zIndex: 5,
  },
  listHeaderWrapper: {
    display: "flex",
    width: "100%",
    flexDirection: "row",
    justifyContent: "space-between",
    marginVertical: Sizing.x5,
  },
  listHeaderText: {
    ...Typography.subHeader.x30,
    alignSelf: "center",
    marginLeft: Sizing.x10,
  },
  addBtn: {
    borderRadius: Outlines.borderRadius.max,
    justifyContent: "center",
    width: Sizing.x40,
    height: Sizing.x40,
    marginHorizontal: Sizing.x10,
    backgroundColor: Colors.primary.s600,
    ...Outlines.shadow.lifted,
  },
  plusIcon: {
    width: Sizing.x25,
    height: Sizing.x25,
    alignSelf: "center",
  },
  arrowIcon: {
    width: Sizing.x70,
    height: Sizing.x70,
    alignSelf: "center",
  },
});
