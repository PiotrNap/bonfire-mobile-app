import * as R from "react"
import { getFromEncryptedStorage } from "lib/encryptedStorage"

export const useAuthCredentials = () => {
  const [credentials, setCredentials] = R.useState<any>(null)

  R.useEffect(() => {
    ;(async () => {
      const _credentials = await getFromEncryptedStorage("auth-credentials")
      if (_credentials) setCredentials(_credentials)
    })()
  }, [])

  return credentials
}
