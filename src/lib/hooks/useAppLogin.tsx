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
        let authCred = await getFromEncryptedStorage("auth-credentials")
        let sec = await getFromEncryptedStorage("privKey")
        let pub = await getFromEncryptedStorage("pubKey")
        let userSettings = await getFromEncryptedStorage("user-settings")
        const isExpired = new Date() > new Date(authCred?.expiresAt)

        console.log("Auth Cred: ", authCred)
        if (!pub) throw new Error(`Missing public key. User not authorized`)

        if (authCred && !isExpired) {
          setAuthorizationToken(authCred.accessToken)
          setIsAuthorized(true)
          setUser({
            username: authCred.username,
            profileType: authCred.profileType,
            id: authCred.id,
          })
        }
        // const tz = await TZ.getTimeZone()

        if (sec && pub) {
          const accessTokenDto = await startChallengeSequence(pub, authCred.id)

          if (accessTokenDto) {
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
      } catch (e) {
        console.error(e)
        setIsAuthorized(false)
      }
      setIsAuthLoaded(true)
    })()
  }, [])

  return { isAuthorized, isAuthLoaded, user }
}
