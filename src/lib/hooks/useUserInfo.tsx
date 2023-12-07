import * as R from "react"

import { Users } from "Api/Users"
import { ProfileContext } from "contexts/profileContext"
import { bufferToBase64 } from "lib/utils"

export const useUserInfo = () => {
  const [isLoading, setIsLoading] = R.useState<boolean>(true)
  const {
    id, // we should have this from our auth-credentials in secure storage
    setName,
    setUsername,
    setID,
    setBio,
    setSkills,
    setJobTitle,
    setImageBase64,
    setProfession,
    setHourlyRateAda,
  } = R.useContext(ProfileContext)

  const updateUser = (val: any) => {
    setName(val.name)
    setUsername(val.username)
    setImageBase64(bufferToBase64(val.profileImage?.data))
    setID(val.id)
    setBio(val.bio)
    setSkills(val.skills)
    setJobTitle(val.jobTitle)
    setProfession(val.prefession)
    setHourlyRateAda(val.hourlyRateAda)
  }

  R.useEffect(() => {
    if (id)
      (async () => {
        try {
          const res = await Users.getUser(id)
          updateUser(res)
        } catch (e) {
          console.error(e)
        } finally {
          setIsLoading(false)
        }
      })()
  }, [id])

  return { isLoading, setIsLoading }
}
