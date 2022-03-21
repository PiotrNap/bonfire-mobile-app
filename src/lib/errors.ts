import { Alert } from "react-native";
import * as Updates from "expo-updates";

export const jsErrorHandler = (e: any, isFatal: any) => {
  if (isFatal) {
    Alert.alert(
      "Unexpected error occured",
      `Error: ${isFatal ? "Fatal:" : ""} ${e.name} ${e.message}
We have already informed our
Gimbalabs dev team!
Please restart the app and try again.
      `,
      [
        {
          text: "Restart",
          /**
           * @description reloadAsync method updates app only
           * when it is ready to be updated. Do not call any meaningfull code
           * after that.
           */
          onPress: async () => Updates.reloadAsync(),
        },
        //@TODO: Remove this button in production
        {
          text: "Cancel",
          onPress: () => {},
          style: "cancel",
        },
      ]
    );
  } else {
    // for seeing the errors in terminal logs
    console.log(e);
  }
};

export const nativeErrorHandler = (errorString: string) => {
  /**
   * @TODO: Do something with the error,
   *        e.g. send the error to our custom API.
   */
};
