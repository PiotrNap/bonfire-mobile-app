import * as R from "react"
import { Events } from "Api/Events"
import { AnyObject } from "yup/lib/types"

export const useEventDeletion = (id: string) => {
  const [errorMsg, setErrorMsg] = R.useState<any>(null)
  const [successMsg, setSuccessMsg] = R.useState<any>(null)
  const [isLoading, setIsLoading] = R.useState<boolean>(false)

  const deleteEvent = async (): Promise<void> => {
    setIsLoading(true)
    setErrorMsg(null)
    setSuccessMsg(null)

    try {
      const success: AnyObject | void = await Events.deleteEvent(id)
      if (success) setSuccessMsg(success.message)
    } catch (e) {
      if (e.message) setErrorMsg(e.message)
    }

    setIsLoading(false)
  }

  return { errorMsg, successMsg, isLoading, deleteEvent }
}
