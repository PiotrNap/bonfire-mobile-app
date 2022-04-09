import * as R from "react"

import { Users } from "Api/Users"

export const useUserInfo = (id: string) => {
  const [userInfo, setUserInfo] = R.useState<any>()

  R.useEffect(() => {
    if (id)
      (async () => {
        try {
          const res = await Users.getUser(id)
          res && setUserInfo(res)
        } catch (e) {
          console.error(e)
        }
      })()
  }, [id])

  return { userInfo }
}
