import * as React from "react"

import { setAuthorizationToken } from "Api/base"
import {
  getFromEncryptedStorage,
  setToEncryptedStorage,
} from "lib/encryptedStorage"
import { startChallengeSequence } from "lib/helpers"
import TZ from "react-native-timezone"

export const useAppLogin = () => {
  const [isAuthorized, setIsAuthorized] = React.useState<boolean>(false)
  const [isAuthLoaded, setIsAuthLoaded] = React.useState<boolean>(false)
  const [user, setUser] = React.useState<any>(null)
  // const isExpired = (expiration: Date) => expiration > new Date()

  React.useEffect(() => {
    ;(async () => {
      try {
        let at = await getFromEncryptedStorage("auth-credentials")
        let sec = await getFromEncryptedStorage("privKey")
        let pub = await getFromEncryptedStorage("pubKey")
        let userSettings = await getFromEncryptedStorage("user-settings")
        const isExpired = new Date() > new Date(at?.expiresAt)

        console.log(at, isExpired)

        if (at && !isExpired) {
          setAuthorizationToken(at.accessToken)
          setIsAuthorized(true)
          setUser({
            username: at.username,
            profileType: at.profileType,
            id: at.id,
          })
        }
        const tz = await TZ.getTimeZone()

        if (sec && pub) {
          const accessTokenDto = await startChallengeSequence(pub, false)

          if (accessTokenDto) {
            console.log(
              "access token? ",
              accessTokenDto.accessToken,
              accessTokenDto.username
            )
            setUser({
              username: accessTokenDto.username,
              profileType: accessTokenDto.profileType,
              id: accessTokenDto.id,
              hourlyRate: accessTokenDto?.hourlyRate,
              timeZone: accessTokenDto?.timeZone,
              userSettings,
            })
            setToEncryptedStorage("auth-credentials", accessTokenDto)
            setAuthorizationToken(accessTokenDto.accessToken)
            setIsAuthorized(true)
          }
        } else {
          if (isAuthorized) setIsAuthorized(false)
        }

        setIsAuthLoaded(true)
      } catch (e) {
        console.error(e)
        setIsAuthLoaded(true)
      }
    })()
  }, [])

  return { isAuthorized, isAuthLoaded, user }
}
