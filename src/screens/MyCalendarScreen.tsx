import * as React from "react";
import { StackScreenProps } from "@react-navigation/stack";
import { AppStackParamList } from "common/types/navigationTypes";
import { Calendar } from "containers/MyCalendar";
import { ErrorHandler } from "components/errors/errorHandler";

export const MyCalendarScreen = ({ navigation }: any) => {
  return (
    <ErrorHandler>
      <Calendar />
    </ErrorHandler>
  );
};
