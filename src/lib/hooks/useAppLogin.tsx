import * as React from "react"
// import {Intl} from 'react-native'

import { setAuthorizationToken } from "Api/base"
import { getFromEncryptedStorage, setToEncryptedStorage } from "lib/encryptedStorage"
import { showErrorToast, startChallengeSequence } from "lib/helpers"
import AsyncStorage from "@react-native-async-storage/async-storage"
import { getDevicesTimeZone } from "lib/utils"

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
        const deviceTimeZone = getDevicesTimeZone()

        // const
        // const tz1 = 'Europe/Paris'
        // const tz2 = 'America/New_York'

        // take regular date string and convert to UTC-4 & UTC+4

        console.log(`
        retrieving from encrypted storage:
            authCred: ${authCred}
            deviceID: ${deviceID}
            devicePubKey: ${pubKey}
            deviceSecKey: ${privKey}
            baseAddress: ${walletBaseAddress}
            timeZone: ${deviceTimeZone}
            `)

        if (typeof authCred == "string") authCred = JSON.parse(authCred)
        if (authCred && !isExpired) {
          setAuthorizationToken(authCred.accessToken)
          setIsAuthorized(true)
          setUser({
            username: authCred.username,
            id: authCred.id,
            deviceID,
            hourlyRateAda: authCred.hourlyRateAda,
            timeZone: deviceTimeZone,
            baseAddress: walletBaseAddress,
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
              timeZone: deviceTimeZone,
              hourlyRateAda: accessTokenDto?.hourlyRateAda,
              userSettings,
              deviceID,
              baseAddress: walletBaseAddress,
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
