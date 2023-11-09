import * as React from "react"

import { setAuthorizationToken } from "Api/base"
import { getFromEncryptedStorage, setToEncryptedStorage } from "lib/encryptedStorage"
import { showErrorToast, startChallengeSequence } from "lib/helpers"
import AsyncStorage from "@react-native-async-storage/async-storage"

export const useAppLogin = () => {
  const [isAuthorized, setIsAuthorized] = React.useState<boolean>(false)
  const [isAuthLoaded, setIsAuthLoaded] = React.useState<boolean>(false)
  const [user, setUser] = React.useState<any>(null)

  React.useEffect(() => {
    ;(async () => {
      try {
        let authCred = await getFromEncryptedStorage("auth-credentials")
        let privKey = await getFromEncryptedStorage("device-privKey")
        let pubKey = await getFromEncryptedStorage("device-pubKey")
        let deviceID = await getFromEncryptedStorage("device-id")
        let userSettings = await getFromEncryptedStorage("user-settings")
        let walletBaseAddress = await AsyncStorage.getItem("account-#0-baseAddress")
        const isExpired = new Date() >= new Date(authCred?.expiresAt)

        console.log(`
        retrieving from encrypted storage:
            authCred: ${authCred}
            deviceID: ${deviceID}
            devicePubKey: ${pubKey}
            deviceSecKey: ${privKey}
            baseAddress: ${walletBaseAddress}
            `)

        if (typeof authCred == "string") authCred = JSON.parse(authCred)
        if (authCred && !isExpired) {
          setAuthorizationToken(authCred.accessToken)
          setIsAuthorized(true)
          setUser({
            username: authCred.username,
            id: authCred.id,
            deviceID,
          })
        }

        if (privKey && pubKey && deviceID) {
          const accessTokenDto = await startChallengeSequence(
            privKey,
            deviceID,
            authCred.id
          )

          if (accessTokenDto) {
            setUser({
              username: accessTokenDto.username,
              id: accessTokenDto.id,
              timeZone: accessTokenDto?.timeZone,
              userSettings,
              deviceID,
            })
            setToEncryptedStorage("auth-credentials", accessTokenDto)
            setAuthorizationToken(accessTokenDto.accessToken)
            setIsAuthorized(true)
          }
        } else {
          if (isAuthorized) setIsAuthorized(false)
        }
      } catch (e) {
        showErrorToast(e)
        setIsAuthorized(false)
      }
      setIsAuthLoaded(true)
    })()
  }, [])

  return { isAuthorized, isAuthLoaded, user }
}
