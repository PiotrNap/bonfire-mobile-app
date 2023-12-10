import * as R from "react"

import { Users } from "Api/Users"
import { UserBaseDTO } from "interfaces/profileInterface"
import { ProfileContext } from "contexts/profileContext"
import { bufferToBase64 } from "lib/utils"
import { getFromEncryptedStorage, setToEncryptedStorage } from "lib/encryptedStorage"

export const useUpdateAccountInfo = () => {
  const [isLoading, setIsLoading] = R.useState<boolean>(false)
  const [isUpdated, setIsUpdated] = R.useState<boolean>(false)
  const [error, setError] = R.useState<boolean>(false)
  const [msg, setMsg] = R.useState<any>(null)
  const {
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

  const updateOrganizer = (val: any) => {
    setName(val.name)
    setUsername(val.username)
    setImageBase64(bufferToBase64(val.profileImage?.data))
    setID(val.id)
    setBio(val.bio)
    setSkills(val.skills)
    setJobTitle(val.jobTitle)
    setProfession(val.profession)
    setHourlyRateAda(val.hourlyRateAda)
  }
  const updateAttendee = (val: any) => {
    setName(val.name)
    setUsername(val.username)
    setImageBase64(bufferToBase64(val.profileImage?.data))
    setID(val.id)
  }

  const updateAccountInfo = async (values: UserBaseDTO, id: string) => {
    setMsg(null)
    setError(false)
    setIsUpdated(false)

    try {
      const oldAuthCred = await getFromEncryptedStorage("auth-credentials")
      let newAuthCred = {
        ...oldAuthCred,
        username: values.username || oldAuthCred.username,
        name: values.name || oldAuthCred.name,
      }

      // if ((values as OrganizerInfo).hourlyRate)
      //   newAuthCred.hourlyRate = (values as OrganizerInfo).hourlyRate
      // await setToEncryptedStorage("auth-credentials", newAuthCred)

      const data = await Users.updateUser(values, id)
      setMsg(data.message)

      if (data.record) {
        if (data.record.profileType === "organizer") {
          updateOrganizer(data.record)
        } else {
          updateAttendee(data.record)
        }
        setMsg(data.message)
        setIsUpdated(true)
        setIsLoading(false)

        return { msg: data.message, status: data.status }
      }
    } catch (e) {
      throw e
    }
  }

  return {
    setIsLoading,
    isLoading,
    isUpdated,
    updateAccountInfo,
    error,
    msg,
  }
}
