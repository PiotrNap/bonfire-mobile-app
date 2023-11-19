#!/bin/bash

# Get the names of the first two AVDs
AVDS=($(emulator -list-avds))
if [ ${#AVDS[@]} -lt 2 ]; then
    echo "Error: Need at least two AVDs to run this script"
    exit 1
fi

# Start emulators with the read-only flag
emulator -avd "${AVDS[0]}" -read-only -port 5554 &
EMULATOR_PID1=$!
emulator -avd "${AVDS[1]}" -read-only -port 5556 &
EMULATOR_PID2=$!

# Function to wait for an emulator to boot
wait_for_emulator() {
    local port=$1
    local boot_completed="no"
    while [ "$boot_completed" != "yes" ]; do
        boot_completed=$(adb -s emulator-$port shell getprop sys.boot_completed 2>&1 | tr -d '\r')
        echo "Waiting for emulator-$port to boot..."
        sleep 1
    done
}

# Wait for each emulator to fully boot
wait_for_emulator 5554
echo "${AVDS[0]} booted"
wait_for_emulator 5556
echo "${AVDS[1]} booted"

# Get device identifiers of the running emulators
DEVICES=($(adb devices | grep emulator | cut -f1))

# Run adb reverse command for each emulator
adb -s "${DEVICES[0]}" reverse tcp:8000 tcp:8000
adb -s "${DEVICES[1]}" reverse tcp:8000 tcp:8000

# Start react-native project
npx react-native start
