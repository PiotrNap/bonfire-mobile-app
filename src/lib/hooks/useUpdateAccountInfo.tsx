import * as R from "react"

import { Users } from "Api/Users"
import { UserBaseDTO, UserOrganizerDTO } from "interfaces/profileInterface"

type AttendeeInfo = UserBaseDTO
type OrganizerInfo = AttendeeInfo & UserOrganizerDTO

export const useUpdateAccountInfo = () => {
  const [isLoading, setIsLoading] = R.useState<boolean>(false)
  const [isUpdated, setIsUpdated] = R.useState<boolean>(false)
  const [error, setError] = R.useState<any>(null)

  const updateAccountInfo = (
    values: AttendeeInfo | OrganizerInfo,
    id: string
  ) => {
    setIsUpdated(false)
    setIsLoading(true)
    update(values, id)
  }

  const update = async (v: any, id: string) => {
    try {
      await Users.updateUser(v, id)
      setIsUpdated(true)
    } catch (err) {
      setError(err)
    }
    setIsLoading(false)
  }

  return { isLoading, isUpdated, updateAccountInfo, error }
}
