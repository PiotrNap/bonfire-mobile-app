#!/bin/bash

# Get the names of the first two AVDs
AVDS=($(emulator -list-avds))
if [ ${#AVDS[@]} -lt 2 ]; then
    echo "Error: Need at least two AVDs to run this script"
    exit 1
fi

# start emulators
emulator -avd "${AVDS[0]}" &
emulator -avd "${AVDS[1]}" &

# wait for emulators to fully boot
adb wait-for-device &&
echo "${AVDS[0]} booted"

adb wait-for-device &&
echo "${AVDS[1]} booted"

# Get device identifiers of the running emulators
DEVICES=($(adb devices | grep emulator | cut -f1))

# run adb reverse command for each emulator
adb -s "${DEVICES[0]}" reverse tcp:8000 tcp:8000
adb -s "${DEVICES[1]}" reverse tcp:8000 tcp:8000

# start react-native project
npx react-native start
