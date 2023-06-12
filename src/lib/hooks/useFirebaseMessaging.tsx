import * as R from "react"

import messaging from "@react-native-firebase/messaging"
import { Users } from "Api/Users"
import { isAndroid } from "lib/helpers"
import {
  getFromEncryptedStorage,
  setToEncryptedStorage,
} from "lib/encryptedStorage"

export const useFirebaseMessaging = (userId: string, isAuthorized: boolean) => {
  const [token, setToken] = R.useState<string | null>(null)
  //TODO on iOS ask for permission before sending any notification
  const askForPermission = () => {}

  const getToken = async (): Promise<void> => {
    let messagingToken = await messaging().getToken()

    const oldToken = await getFromEncryptedStorage("messaging-token")
    if (oldToken && oldToken !== messagingToken) {
      let newTokens: string[] =
        typeof oldToken === "string"
          ? [oldToken, messagingToken]
          : [...oldToken, messagingToken]
      await setToEncryptedStorage("messaging-token", newTokens)
      await updateUserToken(newTokens)
    }

    if (!oldToken) {
      await setToEncryptedStorage("messaging-token", messagingToken)
      await updateUserToken(messagingToken)
    }

    setToken(messagingToken)
  }
  const updateUserToken = async (token: string | string[]) => {
    try {
      await Users.updateUser({ messagingToken: token }, userId)
    } catch (e) {
      console.error(e)
    }
  }

  R.useEffect(() => {
    if (!isAuthorized || !userId) return
    ;(async () => await getToken())()

    // subscribe to all topics (?)
    let topicSubscribe = messaging().subscribeToTopic("Event-booked")

    const messageSubscribe = messaging().onMessage(async (remoteMsg) => {
      if (!remoteMsg || !remoteMsg.notification) return
      console.log(remoteMsg)
      const { notification, data } = remoteMsg

      const { title, body } = notification
      const img = isAndroid ? notification.android?.imageUrl : null

      console.log(remoteMsg)
    })

    const refreshSubscribe = messaging().onTokenRefresh(
      async (token) => await updateUserToken(token)
    )
    return () => {
      messageSubscribe?.()
      refreshSubscribe?.()
      topicSubscribe
    }
  }, [userId, isAuthorized])

  return { token }
}
