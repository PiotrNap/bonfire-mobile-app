import Toast, { ErrorToast, InfoToast, SuccessToast } from "react-native-toast-message"
import { Typography } from "styles/index"

export const ToastMessage = () => {
  const toastConfig = {
    /*
    Overwrite 'error' type,
    by modifying the existing `ErrorToast` component
  */
    info: (props) => (
      <InfoToast
        {...props}
        text1Style={{
          ...Typography.header.x25,
        }}
        text2Style={{
          ...Typography.body.x10,
        }}
      />
    ),
    error: (props) => (
      <ErrorToast
        {...props}
        text1Style={{
          ...Typography.header.x25,
        }}
        text2Style={{
          ...Typography.body.x10,
        }}
      />
    ),
    success: (props) => (
      <SuccessToast
        {...props}
        text1Style={{
          ...Typography.header.x25,
        }}
        text2Style={{
          ...Typography.body.x10,
        }}
      />
    ),
  }

  return <Toast topOffset={20} config={toastConfig} />
}
