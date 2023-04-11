#!/bin/bash

# Get a list of connected devices and extract the serial numbers
devices=$(adb devices | grep -v List | cut -f1)

# Loop through each device and run the adb command
for device in $devices
do
  adb -s $device reverse tcp:8000 tcp:8000
done
