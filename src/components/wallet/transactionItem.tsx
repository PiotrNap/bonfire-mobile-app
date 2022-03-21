import * as React from "react";
import { View, Text, StyleSheet, Image } from "react-native";

// FastImage seem not to work with static resources (?)
// import { FastImage } from "react-native-fast-image";
import { Typography, Colors, Sizing, Outlines } from "styles/index";
import { Transaction } from "common/interfaces/appInterface";

//@ts-ignore
import ProfilePic from "assets/images/profilePicTwo.png";
import { appContext } from "contexts/contextApi";
import { getDate, getDay, getMonth } from "lib/utils";
import { months } from "common/types/calendarTypes";

export interface TransactionItemInterface {
  item: Transaction;
}

export const TransactionItem = ({ item }: TransactionItemInterface) => {
  const { colorScheme } = appContext();
  const { withUser, oldUtxo, newUtxo, date } = item;

  const txAmount = newUtxo - oldUtxo;

  return (
    <View style={styles.container}>
      <Image source={ProfilePic} style={styles.transactionPic} />
      <View style={styles.transactionDetails}>
        <Text
          style={
            colorScheme === "light"
              ? styles.userName_light
              : styles.userName_dark
          }>
          {withUser}
        </Text>
        <Text
          style={
            colorScheme === "light"
              ? styles.transactionInfo_light
              : styles.transactionInfo_dark
          }>
          {newUtxo} ₳ - {months[getMonth(date)]} {getDate(date)}
        </Text>
      </View>
      <Text
        style={[
          styles.amount,
          { color: txAmount < 0 ? "#EF4444" : "#10B981" },
        ]}>
        {txAmount >= 0 && "+"}
        {txAmount} ₳
      </Text>
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    flexDirection: "row",
    alignItems: "center",
    marginBottom: Sizing.x25,
  },
  transactionPic: {
    borderRadius: 999,
    width: Sizing.x55,
    height: Sizing.x55,
  },
  transactionDetails: {
    marginLeft: Sizing.x15,
  },
  userName_light: {
    ...Typography.header.x30,
    color: Colors.primary.s800,
  },
  userName_dark: {
    ...Typography.header.x30,
    color: Colors.primary.neutral,
  },
  transactionInfo_light: {
    ...Typography.subHeader.x20,
    color: Colors.primary.s600,
  },
  transactionInfo_dark: {
    ...Typography.subHeader.x20,
    color: Colors.primary.s180,
  },
  amount: {
    marginLeft: "auto",
    ...Typography.header.x35,
  },
});
