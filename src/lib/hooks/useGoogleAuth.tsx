import * as React from "react"
import { Linking } from "react-native"

import { AxiosResponse } from "axios"
import { InAppBrowser } from "react-native-inappbrowser-reborn"
import * as qs from "qs"

import { Auth } from "Api/Auth"
import { getDeepLinkUri } from "lib/utils"
import { appContext } from "contexts/contextApi"
import axios from "Api/base"

export const useGoogleAuth = (
  navigationCallback: () => void,
  setError: any
) => {
  const { setValidGoogleOAuth } = appContext()
  const [isRequesting, setIsRequesting] = React.useState<boolean>(false)
  const [isInitialRequesting, setIsInitialRequesting] =
    React.useState<boolean>(true)

  React.useEffect(() => {
    ;(async () => {
      try {
        const isValid = await Auth.checkForGoogleAuth()
        setValidGoogleOAuth(!!isValid)
        setIsInitialRequesting(false)
      } catch (e) {
        setIsInitialRequesting(false)
      }
    })()

    const subscription = Linking.addEventListener("url", eventListener)
    return () => subscription.remove()
  }, [])

  const eventListener = React.useCallback((event: { url: string }) => {
    const query = qs.parse(event.url.split("?")[1])
    const { success } = query

    if (success === "false") {
      return setError({ isVisible: true, type: "GoogleOauth" })
    }

    if (success === "true") {
      setValidGoogleOAuth(true)
      navigationCallback()
    }
  }, [])

  const startGoogleAuthentication = async (callbackPath: string) => {
    setIsRequesting(true)
    try {
      const uri = getDeepLinkUri(callbackPath)
      const res: AxiosResponse<any> = await axios.get("auth/google-oauth-url", {
        params: { uri },
      })

      const { authUrl } = await res.data
      // check if device supports this
      const inAppBrowserAvailable = await InAppBrowser.isAvailable()

      if (authUrl && inAppBrowserAvailable) {
        await InAppBrowser.open(authUrl)
      } else Linking.openURL(authUrl)
    } catch (e) {
      console.error(e)
    }

    return setIsRequesting(false)
  }

  return {
    isRequesting,
    isInitialRequesting,
    startGoogleAuthentication,
  }
}
