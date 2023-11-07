import { Alert } from "react-native"
import { isAndroid } from "./helpers"

export const showInappropriateContentModal = () =>
  showFailedModal(
    "Looks like some of the fields contain inappropriate content. Please edit those.",
    "Couldn't proceed"
  )

export const showNSFWImageModal = () => {
  showFailedModal(
    "The image that you've provided was deemed as inappropriate by us. Please, try a different one.",
    "Couldn't proceed"
  )
}

export const showFailedModal = (message: string, title?: string) => {
  Alert.alert(
    title || "Something went wrong",
    message,
    [
      {
        text: "Close",
        style: "cancel",
        onPress: () => {},
      },
    ],
    isAndroid ? { cancelable: true } : {}
  )
}

export const showCredentialsLossWarningModal = (
  onPressCb: () => Promise<void>
) => {
  Alert.alert(
    "Beware",
    "This action is irreversible, you will lose access to your current wallet and account credentials on this device.",
    [
      {
        text: "Yes, I understand",
        style: "destructive",
        onPress: async () => await onPressCb(),
      },
      {
        text: "Cancel",
        style: "cancel",
        onPress: () =>
          Alert.alert(
            "You are safe",
            "None of your credentials were removed. You can continue to enjoy this application."
          ),
      },
    ],
    isAndroid ? { cancelable: true } : {}
  )
}

export const showAccountDeletionWarningModal = (
  onPressCb: () => Promise<void>
) => {
  Alert.alert(
    "Beware",
    "This action is irreversible. You will lose access to your account, and other users won't be able to book your events anymore.",
    [
      {
        text: "Yes, I understand",
        style: "destructive",
        onPress: async () => await onPressCb(),
      },
      {
        text: "Cancel",
        style: "cancel",
        onPress: () =>
          Alert.alert(
            "Your account is safe",
            "You can continue using this application."
          ),
      },
    ],
    isAndroid ? { cancelable: true } : {}
  )
}
