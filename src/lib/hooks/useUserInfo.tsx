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
    setId,
    setBio,
    setSkills,
    setJobTitle,
    setImageBase64,
    setProfession,
    setHourlyRate,
  } = R.useContext(ProfileContext)

  const updateOrganizer = (val: any) => {
    setName(val.name)
    setUsername(val.username)
    setImageBase64(bufferToBase64(val.profileImage?.data))
    setId(val.id)
    setBio(val.bio)
    setSkills(val.skills)
    setJobTitle(val.jobTitle)
    setProfession(val.prefession)
    setHourlyRate(val.hourlyRate)
  }
  const updateAttendee = (val: any) => {
    setName(val.name)
    setUsername(val.username)
    setImageBase64(bufferToBase64(val.profileImage?.data))
    setId(val.id)
  }

  R.useEffect(() => {
    if (id)
      (async () => {
        try {
          const res = await Users.getUser(id)
          if (res?.profileType === "organizer") updateOrganizer(res)
          if (res?.profileType === "attendee") updateAttendee(res)
        } catch (e) {
          console.error(e)
        }
        setIsLoading(false)
      })()
  }, [id])

  return { isLoading }
}
